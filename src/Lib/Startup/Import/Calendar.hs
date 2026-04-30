{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Startup.Import.Calendar
  ( runCalendarStartupImport
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Data.List (sort, sortOn)
import qualified Data.Set as Set
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection, execute, query_)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import qualified AgendaModel as Agenda
import Helpers (tryExcept, withResourceMHandled)
import Lib.Startup.Import.Common (withErrPrefix)
import qualified Lib.Startup.Import.Session as StartupImportSession
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

runCalendarStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runCalendarStartupImport pool cd = do
  let calendarBaseDir = cd </> "data" </> "calendar-items"
  fsEntries <- loadFilesystemCalendarImportEntries calendarBaseDir
  (importedCount, skippedCount, totalPgRows) <-
    withResourceMHandled
      (\err -> "Calendar startup import failed while connecting to Postgres: " ++ show err)
      pool
      (\conn -> do
        pgRows <- tryExcept (query_ conn "SELECT user_id, item_id FROM calendar_items" :: IO [(String, String)])
                            (\err -> "Calendar startup import failed while reading Postgres items: " ++ show err)
        let orderedFsEntries = sortOn (\(userId, itemId, _) -> (userId, itemId)) fsEntries
            knownKeys = Set.fromList pgRows
        when (not (null orderedFsEntries) && not (null pgRows)) $
          lift $ putStrLn
            ( "[startup][calendar-import][warning] overlap detected:"
                ++ " filesystem_count="
                ++ show (length orderedFsEntries)
                ++ " postgres_count="
                ++ show (length pgRows)
                ++ " conflict_policy=postgres-wins"
            )
        (i, s, _) <- foldM (importSingleCalendarItem conn) (0 :: Int, 0 :: Int, knownKeys) orderedFsEntries
        pure (i, s, length pgRows)
      )
  lift $ putStrLn
    ( "[startup][calendar-import] completed"
        ++ " filesystem_count="
        ++ show (length fsEntries)
        ++ " postgres_count="
        ++ show totalPgRows
        ++ " imported="
        ++ show importedCount
        ++ " skipped_conflicts="
        ++ show skippedCount
    )

importSingleCalendarItem
  :: Connection
  -> (Int, Int, Set.Set (String, String))
  -> (String, String, Agenda.CalendarItemContent)
  -> ExceptT String IO (Int, Int, Set.Set (String, String))
importSingleCalendarItem conn (importedCount, skippedCount, knownKeys) (userId, itemId, content) = do
  let key = (userId, itemId)
  if Set.member key knownKeys
    then do
      lift $ putStrLn ("[startup][calendar-import][warning] skipping conflicting user_id=" ++ userId ++ " item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownKeys)
    else do
      let row = calendarContentToDbRow content
      affected <- tryExcept
        (execute conn
          "INSERT INTO calendar_items (user_id, item_id, item_kind, item_type, title, window_start, window_end, status, source_item_id, actual_duration_minutes, category, recurrence_rule_type, recurrence_interval_days, recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (user_id, item_id) DO NOTHING"
          ( userId
          , itemId
          , dbCalendarItemKind row
          , dbCalendarLegacyItemType row
          , dbCalendarLegacyTitle row
          , dbCalendarLegacyWindowStart row
          , dbCalendarLegacyWindowEnd row
          , dbCalendarLegacyStatus row
          , dbCalendarLegacySourceItemId row
          , dbCalendarLegacyActualDurationMinutes row
          , dbCalendarLegacyCategory row
          , dbCalendarLegacyRecurrenceRuleType row
          , dbCalendarLegacyRecurrenceIntervalDays row
          , dbCalendarLegacyRecurrenceExceptionDates row
          , dbCalendarTripWindowStart row
          , dbCalendarTripWindowEnd row
          , dbCalendarTripDeparturePlaceId row
          , dbCalendarTripArrivalPlaceId row
          ))
        (\err -> "Calendar startup import failed while writing user_id=" ++ userId ++ " item_id=" ++ itemId ++ ": " ++ show err)
      if affected > 0
        then pure (importedCount + 1, skippedCount, Set.insert key knownKeys)
        else do
          lift $ putStrLn ("[startup][calendar-import][warning] skipping conflicting user_id=" ++ userId ++ " item_id=" ++ itemId ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert key knownKeys)

loadFilesystemCalendarImportEntries :: FilePath -> ExceptT String IO [(String, String, Agenda.CalendarItemContent)]
loadFilesystemCalendarImportEntries calendarBaseDir = do
  baseExists <- lift $ doesDirectoryExist calendarBaseDir
  if not baseExists
    then do
      lift $ putStrLn "[startup][calendar-import] source calendar directory is missing; treating filesystem calendar source as empty"
      pure []
    else do
      entries <- lift $ listDirectory calendarBaseDir
      foldM (loadSingleCalendarUserDirectory calendarBaseDir) [] (sort entries)

loadSingleCalendarUserDirectory
  :: FilePath
  -> [(String, String, Agenda.CalendarItemContent)]
  -> FilePath
  -> ExceptT String IO [(String, String, Agenda.CalendarItemContent)]
loadSingleCalendarUserDirectory calendarBaseDir acc userId = do
  let userDir = calendarBaseDir </> userId
  isDir <- lift $ doesDirectoryExist userDir
  if not isDir
    then pure acc
    else do
      items <- withErrPrefix
        ("Calendar startup import failed while reading filesystem items for user_id=" ++ userId ++ ": ")
        (StartupImportSession.decodeJsonDirectory userDir :: ExceptT String IO [Agenda.CalendarItem])
      foldM (extractCalendarImportEntry userId) acc items

extractCalendarImportEntry
  :: String
  -> [(String, String, Agenda.CalendarItemContent)]
  -> Agenda.CalendarItem
  -> ExceptT String IO [(String, String, Agenda.CalendarItemContent)]
extractCalendarImportEntry userId acc item =
  case item of
    Agenda.ServerCalendarItem {Agenda.itemId, Agenda.content} ->
      pure ((userId, itemId, content) : acc)
    Agenda.NewCalendarItem {} ->
      throwE ("Calendar startup import failed while reading filesystem items for user_id=" ++ userId ++ ": expected stored calendar item with id")

data CalendarImportDbRow = CalendarImportDbRow
  { dbCalendarItemKind :: String
  , dbCalendarLegacyItemType :: Maybe String
  , dbCalendarLegacyTitle :: Maybe String
  , dbCalendarLegacyWindowStart :: Maybe String
  , dbCalendarLegacyWindowEnd :: Maybe String
  , dbCalendarLegacyStatus :: Maybe String
  , dbCalendarLegacySourceItemId :: Maybe String
  , dbCalendarLegacyActualDurationMinutes :: Maybe Int
  , dbCalendarLegacyCategory :: Maybe String
  , dbCalendarLegacyRecurrenceRuleType :: Maybe String
  , dbCalendarLegacyRecurrenceIntervalDays :: Maybe Int
  , dbCalendarLegacyRecurrenceExceptionDates :: PGArray String
  , dbCalendarTripWindowStart :: Maybe String
  , dbCalendarTripWindowEnd :: Maybe String
  , dbCalendarTripDeparturePlaceId :: Maybe String
  , dbCalendarTripArrivalPlaceId :: Maybe String
  }

calendarContentToDbRow :: Agenda.CalendarItemContent -> CalendarImportDbRow
calendarContentToDbRow content =
  case content of
    Agenda.CalendarItemContent
      { Agenda.itemType
      , Agenda.title
      , Agenda.windowStart
      , Agenda.windowEnd
      , Agenda.status
      , Agenda.sourceItemId
      , Agenda.actualDurationMinutes
      , Agenda.category
      , Agenda.recurrenceRule
      , Agenda.recurrenceExceptionDates
      } ->
        CalendarImportDbRow
          { dbCalendarItemKind = "task"
          , dbCalendarLegacyItemType = Just (calendarItemTypeToDb itemType)
          , dbCalendarLegacyTitle = Just title
          , dbCalendarLegacyWindowStart = Just windowStart
          , dbCalendarLegacyWindowEnd = Just windowEnd
          , dbCalendarLegacyStatus = Just (calendarItemStatusToDb status)
          , dbCalendarLegacySourceItemId = sourceItemId
          , dbCalendarLegacyActualDurationMinutes = actualDurationMinutes
          , dbCalendarLegacyCategory = category
          , dbCalendarLegacyRecurrenceRuleType = fst (calendarRecurrenceToDb recurrenceRule)
          , dbCalendarLegacyRecurrenceIntervalDays = snd (calendarRecurrenceToDb recurrenceRule)
          , dbCalendarLegacyRecurrenceExceptionDates = PGArray recurrenceExceptionDates
          , dbCalendarTripWindowStart = Nothing
          , dbCalendarTripWindowEnd = Nothing
          , dbCalendarTripDeparturePlaceId = Nothing
          , dbCalendarTripArrivalPlaceId = Nothing
          }
    Agenda.TripCalendarItemContent Agenda.TripItemContent {Agenda.tripWindowStart, Agenda.tripWindowEnd, Agenda.departurePlaceId, Agenda.arrivalPlaceId} ->
      CalendarImportDbRow
        { dbCalendarItemKind = "trip"
        , dbCalendarLegacyItemType = Nothing
        , dbCalendarLegacyTitle = Nothing
        , dbCalendarLegacyWindowStart = Nothing
        , dbCalendarLegacyWindowEnd = Nothing
        , dbCalendarLegacyStatus = Nothing
        , dbCalendarLegacySourceItemId = Nothing
        , dbCalendarLegacyActualDurationMinutes = Nothing
        , dbCalendarLegacyCategory = Nothing
        , dbCalendarLegacyRecurrenceRuleType = Nothing
        , dbCalendarLegacyRecurrenceIntervalDays = Nothing
        , dbCalendarLegacyRecurrenceExceptionDates = PGArray []
        , dbCalendarTripWindowStart = Just tripWindowStart
        , dbCalendarTripWindowEnd = Just tripWindowEnd
        , dbCalendarTripDeparturePlaceId = Just departurePlaceId
        , dbCalendarTripArrivalPlaceId = Just arrivalPlaceId
        }

calendarItemTypeToDb :: Agenda.ItemType -> String
calendarItemTypeToDb Agenda.Intention = "INTENTION"
calendarItemTypeToDb Agenda.ScheduledBlock = "BLOC_PLANIFIE"

calendarItemStatusToDb :: Agenda.ItemStatus -> String
calendarItemStatusToDb Agenda.Todo = "TODO"
calendarItemStatusToDb Agenda.EnCours = "EN_COURS"
calendarItemStatusToDb Agenda.Fait = "FAIT"
calendarItemStatusToDb Agenda.Annule = "ANNULE"

calendarRecurrenceToDb :: Maybe Agenda.RecurrenceRule -> (Maybe String, Maybe Int)
calendarRecurrenceToDb Nothing = (Nothing, Nothing)
calendarRecurrenceToDb (Just recurrenceRule) =
  case recurrenceRule of
    Agenda.RecurrenceDaily -> (Just "DAILY", Nothing)
    Agenda.RecurrenceWeekly -> (Just "WEEKLY", Nothing)
    Agenda.RecurrenceMonthly -> (Just "MONTHLY", Nothing)
    Agenda.RecurrenceYearly -> (Just "YEARLY", Nothing)
    Agenda.RecurrenceEveryXDays intervalDays -> (Just "EVERY_X_DAYS", Just intervalDays)
