{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CalendarRepository
  ( CalendarRepository(..)
  , defaultCalendarRepository
  , filesystemCalendarRepository
  , postgresCalendarRepository
  , calendarPostgresHealthChecks
  ) where

import AgendaStorage
  ( CalendarStorageConfig(..)
  , CalendarStorageError(..)
  , createCalendarItem
  , defaultCalendarStorageConfig
  , deleteCalendarItem
  , getCalendarItems
  , updateCalendarItem
  , updateCalendarItemDuration
  )
import qualified AgendaModel as Agenda
import Control.Monad (when)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Pool (Pool)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , execute
  , query
  , query_
  )
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Repository (RepositoryError(..))
import Helpers (tryExcept, withPoolExceptHandled, withResourceMHandled, mapSqlReadException, mapSqlWriteException)
import Data.Int (Int64)


data CalendarRepository = CalendarRepository
  { repoCreateCalendarItem :: !(String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem)
  , repoLoadCalendarItemById :: !(String -> String -> ExceptT RepositoryError IO Agenda.CalendarItem)
  , repoListCalendarItemsForUser :: !(String -> ExceptT RepositoryError IO [Agenda.CalendarItem])
  , repoUpdateCalendarItem :: !(String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem)
  , repoUpdateCalendarItemDuration :: !(String -> String -> Int -> ExceptT RepositoryError IO Agenda.CalendarItem)
  , repoDeleteCalendarItemById :: !(String -> String -> ExceptT RepositoryError IO ())
  }

defaultCalendarRepository :: CalendarRepository
defaultCalendarRepository = filesystemCalendarRepository defaultCalendarStorageConfig

filesystemCalendarRepository :: CalendarStorageConfig -> CalendarRepository
filesystemCalendarRepository config =
  CalendarRepository
    { repoCreateCalendarItem = fsCreateCalendarItem config
    , repoLoadCalendarItemById = fsLoadCalendarItemById config
    , repoListCalendarItemsForUser = fsListCalendarItemsForUser config
    , repoUpdateCalendarItem = fsUpdateCalendarItem config
    , repoUpdateCalendarItemDuration = fsUpdateCalendarItemDuration config
    , repoDeleteCalendarItemById = fsDeleteCalendarItemById config
    }

postgresCalendarRepository :: Pool Connection -> CalendarRepository
postgresCalendarRepository pool =
  CalendarRepository
    { repoCreateCalendarItem = pgCreateCalendarItem pool
    , repoLoadCalendarItemById = pgLoadCalendarItemById pool
    , repoListCalendarItemsForUser = pgListCalendarItemsForUser pool
    , repoUpdateCalendarItem = pgUpdateCalendarItem pool
    , repoUpdateCalendarItemDuration = pgUpdateCalendarItemDuration pool
    , repoDeleteCalendarItemById = pgDeleteCalendarItemById pool
    }

calendarPostgresHealthChecks :: Connection -> ExceptT String IO ()
calendarPostgresHealthChecks conn = do
  tryExcept (query_ conn
          "SELECT user_id, item_id, item_kind, item_type, title, window_start, window_end, status, source_item_id, actual_duration_minutes, category, recurrence_rule_type, recurrence_interval_days, recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id FROM calendar_items LIMIT 0"
          :: IO [(String, String, String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe Int, Maybe String, Maybe String, Maybe Int, PGArray String, Maybe String, Maybe String, Maybe String, Maybe String)])
             (\err -> "Calendar schema check failed: " ++ show err)
  pure ()

fsCreateCalendarItem :: CalendarStorageConfig -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
fsCreateCalendarItem config userId content = tryExcept (createCalendarItem config userId content) (const WriteFailure)

fsLoadCalendarItemById :: CalendarStorageConfig -> String -> String -> ExceptT RepositoryError IO Agenda.CalendarItem
fsLoadCalendarItemById config userId itemId = do
  items <- fsListCalendarItemsForUser config userId
  case filter matchesItemId items of
    [] -> throwError NotFound
    (item:_) -> pure item
  where
    matchesItemId item =
      case item of
        Agenda.ServerCalendarItem {Agenda.itemId = sid} -> sid == itemId
        Agenda.NewCalendarItem {} -> False

fsListCalendarItemsForUser :: CalendarStorageConfig -> String -> ExceptT RepositoryError IO [Agenda.CalendarItem]
fsListCalendarItemsForUser config userId = do
  items <- tryExcept (getCalendarItems config userId) (const ReadFailure)
  pure $ sortOn calendarItemSortKey items

fsUpdateCalendarItem :: CalendarStorageConfig -> String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
fsUpdateCalendarItem config userId itemId content = do
  result <- liftIO (updateCalendarItem config userId itemId content)
  case result of
    Left err -> throwError (mapCalendarError err)
    Right item -> pure item

fsUpdateCalendarItemDuration :: CalendarStorageConfig -> String -> String -> Int -> ExceptT RepositoryError IO Agenda.CalendarItem
fsUpdateCalendarItemDuration config userId itemId minutes = do
  result <- liftIO (updateCalendarItemDuration config userId itemId minutes)
  case result of
    Left err -> throwError (mapCalendarError err)
    Right item -> pure item

fsDeleteCalendarItemById :: CalendarStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsDeleteCalendarItemById config userId itemId = do
  result <- liftIO (deleteCalendarItem config userId itemId)
  either (throwError . mapCalendarError) pure result

mapCalendarError :: CalendarStorageError -> RepositoryError
mapCalendarError CalendarItemNotFound = NotFound
mapCalendarError CalendarItemReadFailure = ReadFailure
mapCalendarError CalendarItemWriteFailure = WriteFailure

calendarItemSortKey :: Agenda.CalendarItem -> String
calendarItemSortKey item =
  case item of
    Agenda.ServerCalendarItem {Agenda.itemId} -> itemId
    Agenda.NewCalendarItem {} -> ""

pgCreateCalendarItem :: Pool Connection -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
pgCreateCalendarItem pool userId content =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    itemId <- liftIO (toString <$> nextRandom)
    let row = contentToDbRow content
    writeResult <- tryExcept
      (execute conn
        "INSERT INTO calendar_items (user_id, item_id, item_kind, item_type, title, window_start, window_end, status, source_item_id, actual_duration_minutes, category, recurrence_rule_type, recurrence_interval_days, recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ( userId
        , itemId
        , dbItemKind row
        , dbLegacyItemType row
        , dbLegacyTitle row
        , dbLegacyWindowStart row
        , dbLegacyWindowEnd row
        , dbLegacyStatus row
        , dbLegacySourceItemId row
        , dbLegacyActualDurationMinutes row
        , dbLegacyCategory row
        , dbLegacyRecurrenceRuleType row
        , dbLegacyRecurrenceIntervalDays row
        , dbLegacyRecurrenceExceptionDates row
        , dbTripWindowStart row
        , dbTripWindowEnd row
        , dbTripDeparturePlaceId row
        , dbTripArrivalPlaceId row
        ))
        mapSqlWriteException
    pure Agenda.ServerCalendarItem {Agenda.content = content, Agenda.itemId = itemId}

pgLoadCalendarItemById :: Pool Connection -> String -> String -> ExceptT RepositoryError IO Agenda.CalendarItem
pgLoadCalendarItemById pool userId itemId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    readResult <- tryExcept
      (query conn
        "SELECT user_id, item_id, item_kind, item_type, title, window_start, window_end, status, source_item_id, actual_duration_minutes, category, recurrence_rule_type, recurrence_interval_days, recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id FROM calendar_items WHERE user_id = ? AND item_id = ?"
        (userId, itemId))
        mapSqlReadException
    case readResult of
      [] -> throwError NotFound
      (row:_) ->
        case rowToCalendarItem row of
          Nothing -> throwError ReadFailure
          Just item -> pure item

pgListCalendarItemsForUser :: Pool Connection -> String -> ExceptT RepositoryError IO [Agenda.CalendarItem]
pgListCalendarItemsForUser pool userId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <-tryExcept
      (query conn
        "SELECT user_id, item_id, item_kind, item_type, title, window_start, window_end, status, source_item_id, actual_duration_minutes, category, recurrence_rule_type, recurrence_interval_days, recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id FROM calendar_items WHERE user_id = ? ORDER BY item_id"
        (Only userId))
        mapSqlReadException
    case mapM rowToCalendarItem rows of
      Nothing -> throwError ReadFailure
      Just items -> pure items

pgUpdateCalendarItem :: Pool Connection -> String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
pgUpdateCalendarItem pool userId itemId content =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    let row = contentToDbRow content
    affected <- tryExcept
      (execute conn
        "UPDATE calendar_items SET item_kind = ?, item_type = ?, title = ?, window_start = ?, window_end = ?, status = ?, source_item_id = ?, actual_duration_minutes = ?, category = ?, recurrence_rule_type = ?, recurrence_interval_days = ?, recurrence_exception_dates = ?, trip_window_start = ?, trip_window_end = ?, trip_departure_place_id = ?, trip_arrival_place_id = ? WHERE user_id = ? AND item_id = ?"
        ( dbItemKind row
        , dbLegacyItemType row
        , dbLegacyTitle row
        , dbLegacyWindowStart row
        , dbLegacyWindowEnd row
        , dbLegacyStatus row
        , dbLegacySourceItemId row
        , dbLegacyActualDurationMinutes row
        , dbLegacyCategory row
        , dbLegacyRecurrenceRuleType row
        , dbLegacyRecurrenceIntervalDays row
        , dbLegacyRecurrenceExceptionDates row
        , dbTripWindowStart row
        , dbTripWindowEnd row
        , dbTripDeparturePlaceId row
        , dbTripArrivalPlaceId row
        , userId
        , itemId
        ))
        mapSqlWriteException
    when (affected == 0) (throwError NotFound)
    pure Agenda.ServerCalendarItem {Agenda.content = content, Agenda.itemId = itemId}

pgUpdateCalendarItemDuration :: Pool Connection -> String -> String -> Int -> ExceptT RepositoryError IO Agenda.CalendarItem
pgUpdateCalendarItemDuration pool userId itemId minutes = do
  loaded <- pgLoadCalendarItemById pool userId itemId
  case loaded of
    Agenda.ServerCalendarItem {Agenda.content = Agenda.CalendarItemContent {}} -> do
      withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
        affected <- tryExcept
          (execute conn
            "UPDATE calendar_items SET actual_duration_minutes = ? WHERE user_id = ? AND item_id = ?"
            (minutes, userId, itemId))
            mapSqlWriteException
        when (affected == 0) (throwError NotFound)
      pgLoadCalendarItemById pool userId itemId
    _ -> pure loaded

pgDeleteCalendarItemById :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteCalendarItemById pool userId itemId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    affected <- tryExcept
                     (execute conn "DELETE FROM calendar_items WHERE user_id = ? AND item_id = ?" (userId, itemId))
                     mapSqlWriteException
    when (affected == 0) $ throwError NotFound

data CalendarDbRow = CalendarDbRow
  { dbItemKind :: String
  , dbLegacyItemType :: Maybe String
  , dbLegacyTitle :: Maybe String
  , dbLegacyWindowStart :: Maybe String
  , dbLegacyWindowEnd :: Maybe String
  , dbLegacyStatus :: Maybe String
  , dbLegacySourceItemId :: Maybe String
  , dbLegacyActualDurationMinutes :: Maybe Int
  , dbLegacyCategory :: Maybe String
  , dbLegacyRecurrenceRuleType :: Maybe String
  , dbLegacyRecurrenceIntervalDays :: Maybe Int
  , dbLegacyRecurrenceExceptionDates :: PGArray String
  , dbTripWindowStart :: Maybe String
  , dbTripWindowEnd :: Maybe String
  , dbTripDeparturePlaceId :: Maybe String
  , dbTripArrivalPlaceId :: Maybe String
  }

type CalendarDbRowRaw =
  ( String
  , String
  , String
  , Maybe String
  , Maybe String
  , Maybe String
  , Maybe String
  , Maybe String
  , Maybe String
  , Maybe Int
  , Maybe String
  , Maybe String
  , Maybe Int
  , PGArray String
  , Maybe String
  , Maybe String
  , Maybe String
  , Maybe String
  )

contentToDbRow :: Agenda.CalendarItemContent -> CalendarDbRow
contentToDbRow content =
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
        CalendarDbRow
          { dbItemKind = "task"
          , dbLegacyItemType = Just (itemTypeToDb itemType)
          , dbLegacyTitle = Just title
          , dbLegacyWindowStart = Just windowStart
          , dbLegacyWindowEnd = Just windowEnd
          , dbLegacyStatus = Just (itemStatusToDb status)
          , dbLegacySourceItemId = sourceItemId
          , dbLegacyActualDurationMinutes = actualDurationMinutes
          , dbLegacyCategory = category
          , dbLegacyRecurrenceRuleType = fst (recurrenceToDb recurrenceRule)
          , dbLegacyRecurrenceIntervalDays = snd (recurrenceToDb recurrenceRule)
          , dbLegacyRecurrenceExceptionDates = PGArray recurrenceExceptionDates
          , dbTripWindowStart = Nothing
          , dbTripWindowEnd = Nothing
          , dbTripDeparturePlaceId = Nothing
          , dbTripArrivalPlaceId = Nothing
          }
    Agenda.TripCalendarItemContent Agenda.TripItemContent {Agenda.tripWindowStart, Agenda.tripWindowEnd, Agenda.departurePlaceId, Agenda.arrivalPlaceId} ->
      CalendarDbRow
        { dbItemKind = "trip"
        , dbLegacyItemType = Nothing
        , dbLegacyTitle = Nothing
        , dbLegacyWindowStart = Nothing
        , dbLegacyWindowEnd = Nothing
        , dbLegacyStatus = Nothing
        , dbLegacySourceItemId = Nothing
        , dbLegacyActualDurationMinutes = Nothing
        , dbLegacyCategory = Nothing
        , dbLegacyRecurrenceRuleType = Nothing
        , dbLegacyRecurrenceIntervalDays = Nothing
        , dbLegacyRecurrenceExceptionDates = PGArray []
        , dbTripWindowStart = Just tripWindowStart
        , dbTripWindowEnd = Just tripWindowEnd
        , dbTripDeparturePlaceId = Just departurePlaceId
        , dbTripArrivalPlaceId = Just arrivalPlaceId
        }

rowToCalendarItem :: CalendarDbRowRaw -> Maybe Agenda.CalendarItem
rowToCalendarItem (_userId, itemId, itemKind, legacyItemType, legacyTitle, legacyWindowStart, legacyWindowEnd, legacyStatus, legacySourceItemId, legacyActualDurationMinutes, legacyCategory, legacyRecurrenceRuleType, legacyRecurrenceIntervalDays, PGArray legacyRecurrenceExceptionDates, tripWindowStart, tripWindowEnd, tripDeparturePlaceId, tripArrivalPlaceId) = do
  content <-
    case itemKind of
      "task" -> do
        itemTypeRaw <- legacyItemType
        title <- legacyTitle
        windowStart <- legacyWindowStart
        windowEnd <- legacyWindowEnd
        statusRaw <- legacyStatus
        itemType <- itemTypeFromDb itemTypeRaw
        status <- itemStatusFromDb statusRaw
        recurrenceRule <- recurrenceFromDb legacyRecurrenceRuleType legacyRecurrenceIntervalDays
        pure
          Agenda.CalendarItemContent
            { Agenda.itemType = itemType
            , Agenda.title = title
            , Agenda.windowStart = windowStart
            , Agenda.windowEnd = windowEnd
            , Agenda.status = status
            , Agenda.sourceItemId = legacySourceItemId
            , Agenda.actualDurationMinutes = legacyActualDurationMinutes
            , Agenda.category = legacyCategory
            , Agenda.recurrenceRule = recurrenceRule
            , Agenda.recurrenceExceptionDates = legacyRecurrenceExceptionDates
            }
      "trip" -> do
        tripStart <- tripWindowStart
        tripEnd <- tripWindowEnd
        departure <- tripDeparturePlaceId
        arrival <- tripArrivalPlaceId
        pure
          (Agenda.TripCalendarItemContent Agenda.TripItemContent
            { Agenda.tripWindowStart = tripStart
            , Agenda.tripWindowEnd = tripEnd
            , Agenda.departurePlaceId = departure
            , Agenda.arrivalPlaceId = arrival
            })
      _ -> Nothing
  pure Agenda.ServerCalendarItem {Agenda.content = content, Agenda.itemId = itemId}

itemTypeToDb :: Agenda.ItemType -> String
itemTypeToDb Agenda.Intention = "INTENTION"
itemTypeToDb Agenda.ScheduledBlock = "BLOC_PLANIFIE"

itemTypeFromDb :: String -> Maybe Agenda.ItemType
itemTypeFromDb "INTENTION" = Just Agenda.Intention
itemTypeFromDb "BLOC_PLANIFIE" = Just Agenda.ScheduledBlock
itemTypeFromDb _ = Nothing

itemStatusToDb :: Agenda.ItemStatus -> String
itemStatusToDb Agenda.Todo = "TODO"
itemStatusToDb Agenda.EnCours = "EN_COURS"
itemStatusToDb Agenda.Fait = "FAIT"
itemStatusToDb Agenda.Annule = "ANNULE"

itemStatusFromDb :: String -> Maybe Agenda.ItemStatus
itemStatusFromDb "TODO" = Just Agenda.Todo
itemStatusFromDb "EN_COURS" = Just Agenda.EnCours
itemStatusFromDb "FAIT" = Just Agenda.Fait
itemStatusFromDb "ANNULE" = Just Agenda.Annule
itemStatusFromDb _ = Nothing

recurrenceToDb :: Maybe Agenda.RecurrenceRule -> (Maybe String, Maybe Int)
recurrenceToDb Nothing = (Nothing, Nothing)
recurrenceToDb (Just rule) =
  case rule of
    Agenda.RecurrenceDaily -> (Just "DAILY", Nothing)
    Agenda.RecurrenceWeekly -> (Just "WEEKLY", Nothing)
    Agenda.RecurrenceMonthly -> (Just "MONTHLY", Nothing)
    Agenda.RecurrenceYearly -> (Just "YEARLY", Nothing)
    Agenda.RecurrenceEveryXDays n -> (Just "EVERY_X_DAYS", Just n)

recurrenceFromDb :: Maybe String -> Maybe Int -> Maybe (Maybe Agenda.RecurrenceRule)
recurrenceFromDb Nothing Nothing = Just Nothing
recurrenceFromDb (Just "DAILY") Nothing = Just (Just Agenda.RecurrenceDaily)
recurrenceFromDb (Just "WEEKLY") Nothing = Just (Just Agenda.RecurrenceWeekly)
recurrenceFromDb (Just "MONTHLY") Nothing = Just (Just Agenda.RecurrenceMonthly)
recurrenceFromDb (Just "YEARLY") Nothing = Just (Just Agenda.RecurrenceYearly)
recurrenceFromDb (Just "EVERY_X_DAYS") (Just n) = Just (Just (Agenda.RecurrenceEveryXDays n))
recurrenceFromDb _ _ = Nothing
