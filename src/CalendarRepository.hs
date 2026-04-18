{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CalendarRepository
  ( CalendarRepository(..)
  , defaultCalendarRepository
  , filesystemCalendarRepository
  , postgresCalendarRepository
  , verifyPostgresCalendarStorage
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
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , SqlError(..)
  , close
  , connectPostgreSQL
  , execute
  , query
  , query_
  )
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Repository (RepositoryError(..))
import qualified Control.Exception as Ex
import Data.Int (Int64)


data CalendarRepository = CalendarRepository
  { repoCreateCalendarItem :: String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoLoadCalendarItemById :: String -> String -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoListCalendarItemsForUser :: String -> ExceptT RepositoryError IO [Agenda.CalendarItem]
  , repoUpdateCalendarItem :: String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoUpdateCalendarItemDuration :: String -> String -> Int -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoDeleteCalendarItemById :: String -> String -> ExceptT RepositoryError IO ()
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

postgresCalendarRepository :: String -> CalendarRepository
postgresCalendarRepository connectionString =
  CalendarRepository
    { repoCreateCalendarItem = pgCreateCalendarItem connectionString
    , repoLoadCalendarItemById = pgLoadCalendarItemById connectionString
    , repoListCalendarItemsForUser = pgListCalendarItemsForUser connectionString
    , repoUpdateCalendarItem = pgUpdateCalendarItem connectionString
    , repoUpdateCalendarItemDuration = pgUpdateCalendarItemDuration connectionString
    , repoDeleteCalendarItemById = pgDeleteCalendarItemById connectionString
    }

verifyPostgresCalendarStorage :: String -> IO (Either String ())
verifyPostgresCalendarStorage connectionString = do
  connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
  case connResult of
    Left err -> pure (Left ("Unable to connect to Postgres: " ++ show err))
    Right conn -> do
      pingResult <- Ex.try (query_ conn "SELECT 1" :: IO [Only Int]) :: IO (Either Ex.SomeException [Only Int])
      schemaResult <- Ex.try
        (query_ conn
          "SELECT user_id, item_id, item_kind, legacy_item_type, legacy_title, legacy_window_start, legacy_window_end, legacy_status, legacy_source_item_id, legacy_actual_duration_minutes, legacy_category, legacy_recurrence_rule_type, legacy_recurrence_interval_days, legacy_recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id FROM calendar_items LIMIT 0"
          :: IO [(String, String, String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe Int, Maybe String, Maybe String, Maybe Int, PGArray String, Maybe String, Maybe String, Maybe String, Maybe String)])
        :: IO (Either Ex.SomeException [(String, String, String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe Int, Maybe String, Maybe String, Maybe Int, PGArray String, Maybe String, Maybe String, Maybe String, Maybe String)])
      _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
      case pingResult of
        Left err -> pure (Left ("Postgres ping query failed: " ++ show err))
        Right _ ->
          case schemaResult of
            Left err -> pure (Left ("Calendar schema check failed: " ++ show err))
            Right _ -> pure (Right ())

fsCreateCalendarItem :: CalendarStorageConfig -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
fsCreateCalendarItem config userId content = do
  result <- liftIO (Ex.try (createCalendarItem config userId content) :: IO (Either Ex.IOException Agenda.CalendarItem))
  case result of
    Left _ -> throwError WriteFailure
    Right item -> pure item

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
  result <- liftIO (Ex.try (getCalendarItems config userId) :: IO (Either Ex.IOException [Agenda.CalendarItem]))
  case result of
    Left _ -> throwError ReadFailure
    Right items -> pure (sortOn calendarItemSortKey items)

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
  case result of
    Left err -> throwError (mapCalendarError err)
    Right () -> pure ()

mapCalendarError :: CalendarStorageError -> RepositoryError
mapCalendarError CalendarItemNotFound = NotFound
mapCalendarError CalendarItemReadFailure = ReadFailure
mapCalendarError CalendarItemWriteFailure = WriteFailure

calendarItemSortKey :: Agenda.CalendarItem -> String
calendarItemSortKey item =
  case item of
    Agenda.ServerCalendarItem {Agenda.itemId} -> itemId
    Agenda.NewCalendarItem {} -> ""

pgCreateCalendarItem :: String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
pgCreateCalendarItem connectionString userId content =
  withPgConnection connectionString StorageFailure $ \conn -> do
    itemId <- liftIO (toString <$> nextRandom)
    let row = contentToDbRow content
    writeResult <- liftIO (Ex.try
      (execute conn
        "INSERT INTO calendar_items (user_id, item_id, item_kind, legacy_item_type, legacy_title, legacy_window_start, legacy_window_end, legacy_status, legacy_source_item_id, legacy_actual_duration_minutes, legacy_category, legacy_recurrence_rule_type, legacy_recurrence_interval_days, legacy_recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
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
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure Agenda.ServerCalendarItem {Agenda.content = content, Agenda.itemId = itemId}

pgLoadCalendarItemById :: String -> String -> String -> ExceptT RepositoryError IO Agenda.CalendarItem
pgLoadCalendarItemById connectionString userId itemId =
  withPgConnection connectionString StorageFailure $ \conn -> do
    readResult <- liftIO (Ex.try
      (query conn
        "SELECT user_id, item_id, item_kind, legacy_item_type, legacy_title, legacy_window_start, legacy_window_end, legacy_status, legacy_source_item_id, legacy_actual_duration_minutes, legacy_category, legacy_recurrence_rule_type, legacy_recurrence_interval_days, legacy_recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id FROM calendar_items WHERE user_id = ? AND item_id = ?"
        (userId, itemId))
      :: IO (Either Ex.SomeException [CalendarDbRowRaw]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right [] -> throwError NotFound
      Right (row:_) ->
        case rowToCalendarItem row of
          Nothing -> throwError ReadFailure
          Just item -> pure item

pgListCalendarItemsForUser :: String -> String -> ExceptT RepositoryError IO [Agenda.CalendarItem]
pgListCalendarItemsForUser connectionString userId =
  withPgConnection connectionString StorageFailure $ \conn -> do
    readResult <- liftIO (Ex.try
      (query conn
        "SELECT user_id, item_id, item_kind, legacy_item_type, legacy_title, legacy_window_start, legacy_window_end, legacy_status, legacy_source_item_id, legacy_actual_duration_minutes, legacy_category, legacy_recurrence_rule_type, legacy_recurrence_interval_days, legacy_recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id FROM calendar_items WHERE user_id = ? ORDER BY item_id"
        (Only userId))
      :: IO (Either Ex.SomeException [CalendarDbRowRaw]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right rows ->
        case mapM rowToCalendarItem rows of
          Nothing -> throwError ReadFailure
          Just items -> pure items

pgUpdateCalendarItem :: String -> String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
pgUpdateCalendarItem connectionString userId itemId content =
  withPgConnection connectionString StorageFailure $ \conn -> do
    let row = contentToDbRow content
    writeResult <- liftIO (Ex.try
      (execute conn
        "UPDATE calendar_items SET item_kind = ?, legacy_item_type = ?, legacy_title = ?, legacy_window_start = ?, legacy_window_end = ?, legacy_status = ?, legacy_source_item_id = ?, legacy_actual_duration_minutes = ?, legacy_category = ?, legacy_recurrence_rule_type = ?, legacy_recurrence_interval_days = ?, legacy_recurrence_exception_dates = ?, trip_window_start = ?, trip_window_end = ?, trip_departure_place_id = ?, trip_arrival_place_id = ? WHERE user_id = ? AND item_id = ?"
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
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right affected -> do
        when (affected == 0) (throwError NotFound)
        pure Agenda.ServerCalendarItem {Agenda.content = content, Agenda.itemId = itemId}

pgUpdateCalendarItemDuration :: String -> String -> String -> Int -> ExceptT RepositoryError IO Agenda.CalendarItem
pgUpdateCalendarItemDuration connectionString userId itemId minutes = do
  loaded <- pgLoadCalendarItemById connectionString userId itemId
  case loaded of
    Agenda.ServerCalendarItem {Agenda.content = Agenda.CalendarItemContent {}} -> do
      withPgConnection connectionString StorageFailure $ \conn -> do
        writeResult <- liftIO (Ex.try
          (execute conn
            "UPDATE calendar_items SET legacy_actual_duration_minutes = ? WHERE user_id = ? AND item_id = ?"
            (minutes, userId, itemId))
          :: IO (Either Ex.SomeException Int64))
        case writeResult of
          Left err -> throwError (mapWriteException err)
          Right affected -> when (affected == 0) (throwError NotFound)
      pgLoadCalendarItemById connectionString userId itemId
    _ -> pure loaded

pgDeleteCalendarItemById :: String -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteCalendarItemById connectionString userId itemId =
  withPgConnection connectionString StorageFailure $ \conn -> do
    writeResult <- liftIO (Ex.try
      (execute conn "DELETE FROM calendar_items WHERE user_id = ? AND item_id = ?" (userId, itemId))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right affected -> when (affected == 0) $ throwError NotFound

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
          { dbItemKind = "legacy"
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
      "legacy" -> do
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

withPgConnection :: String -> RepositoryError -> (Connection -> ExceptT RepositoryError IO a) -> ExceptT RepositoryError IO a
withPgConnection connectionString connectionError action = do
  connResult <- liftIO $ Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: ExceptT RepositoryError IO (Either Ex.SomeException Connection)
  case connResult of
    Left _ -> throwError connectionError
    Right conn -> do
      runResult <- liftIO (runExceptT (action conn))
      _ <- liftIO $ Ex.try (close conn) :: ExceptT RepositoryError IO (Either Ex.SomeException ())
      either throwError pure runResult

mapReadException :: Ex.SomeException -> RepositoryError
mapReadException ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr ->
      if isStorageSqlError sqlErr
        then StorageFailure
        else ReadFailure
    Nothing -> StorageFailure

mapWriteException :: Ex.SomeException -> RepositoryError
mapWriteException ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr
      | isUniqueViolation sqlErr -> AlreadyExists
      | isStorageSqlError sqlErr -> StorageFailure
      | otherwise -> WriteFailure
    Nothing -> StorageFailure

isUniqueViolation :: SqlError -> Bool
isUniqueViolation sqlErr = sqlState sqlErr == BS8.pack "23505"

isStorageSqlError :: SqlError -> Bool
isStorageSqlError sqlErr = "08" `BS8.isPrefixOf` sqlState sqlErr
