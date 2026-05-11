{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FinanceTransactionRepository
  ( FinanceTransactionRepository(..)
  , FinanceTransaction(..)
  , FinanceReportDirection(..)
  , FinanceReportRequest(..)
  , FinanceReportResult(..)
  , FinanceTransactionNote(..)
  , FinanceTransactionAdjustment(..)
  , FinanceTransactionNoteCreateRequest(..)
  , FinanceTransactionNoteUpdateRequest(..)
  , FinanceTransactionLinkRequest(..)
  , FinanceTransactionSplitRow(..)
  , FinanceTransactionTransfer(..)
  , FinanceTransactionCreateRequest(..)
  , FinanceTransactionCategorizeRequest(..)
  , FinanceTransactionSplitRequest(..)
  , FinanceTransactionSplitWriteRow(..)
  , FinanceTransactionWriteRequest(..)
  , FinanceTransactionDirection(..)
  , FinanceCanonicalEventEnvelope(..)
  , financeTransactionDirectionText
  , postgresFinanceTransactionRepository
  , financeTransactionPostgresHealthChecks
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, toLower)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value, object, withObject, (.:), (.:?), (.=))
import Data.Int (Int64)
import Data.List (dropWhileEnd, sortOn)
import qualified Data.Map.Strict as Map
import Data.Pool (Pool)
import Data.Ord (Down(..))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , In(..)
  , Only(..)
  , execute
  , query
  , query_
  , withTransaction
  )
import Helpers (mapSqlReadException, mapSqlWriteException, tryExcept, withPoolExceptHandled)
import Repository (RepositoryError(..))

data FinanceTransactionRepository = FinanceTransactionRepository
  { repoCreateFinanceTransaction :: !(String -> FinanceTransactionWriteRequest -> ExceptT RepositoryError IO FinanceTransaction)
  , repoLoadFinanceTransactionById :: !(String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoListFinanceTransactions :: !(String -> Maybe String -> Maybe UTCTime -> Maybe UTCTime -> ExceptT RepositoryError IO [FinanceTransaction])
  , repoGetFinanceReport :: !(String -> FinanceReportRequest -> ExceptT RepositoryError IO FinanceReportResult)
  , repoAddFinanceTransactionNote :: !(String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoUpdateFinanceTransactionNote :: !(String -> String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoDeleteFinanceTransactionNote :: !(String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoCategorizeFinanceTransaction :: !(String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoSplitFinanceTransaction :: !(String -> String -> [FinanceTransactionSplitWriteRow] -> ExceptT RepositoryError IO FinanceTransaction)
  , repoLinkFinanceTransactions :: !(String -> String -> String -> String -> ExceptT RepositoryError IO (FinanceTransaction, FinanceTransaction))
  , repoListFinanceCanonicalEvents :: !(String -> ExceptT RepositoryError IO [FinanceCanonicalEventEnvelope])
  }

data FinanceTransactionCreateRequest = FinanceTransactionCreateRequest
  { financeTransactionCreateAccountId :: !String
  , financeTransactionCreateAmount :: !Int
  , financeTransactionCreateOccurredAt :: !(Maybe String)
  , financeTransactionCreateCounterparty :: !(Maybe String)
  , financeTransactionCreateDescription :: !(Maybe String)
  }

data FinanceTransactionNoteCreateRequest = FinanceTransactionNoteCreateRequest
  { financeTransactionNoteCreateText :: !String
  }

data FinanceTransactionNoteUpdateRequest = FinanceTransactionNoteUpdateRequest
  { financeTransactionNoteUpdateText :: !String
  }

data FinanceTransactionLinkRequest = FinanceTransactionLinkRequest
  { financeTransactionLinkSourceTransactionId :: !String
  , financeTransactionLinkTargetTransactionId :: !String
  , financeTransactionLinkType :: !String
  }

data FinanceTransactionCategorizeRequest = FinanceTransactionCategorizeRequest
  { financeTransactionCategorizeCategory :: !String
  }

data FinanceTransactionSplitRequest = FinanceTransactionSplitRequest
  { financeTransactionSplitRows :: ![FinanceTransactionSplitWriteRow]
  }

data FinanceTransactionSplitWriteRow = FinanceTransactionSplitWriteRow
  { financeTransactionSplitWriteAmount :: !Int
  , financeTransactionSplitWriteCategory :: !String
  } deriving (Eq, Show)

data FinanceTransactionWriteRequest = FinanceTransactionWriteRequest
  { financeTransactionWriteIdempotencyKey :: !String
  , financeTransactionWriteDirection :: !FinanceTransactionDirection
  , financeTransactionWriteAccountId :: !String
  , financeTransactionWriteAmount :: !Int
  , financeTransactionWriteOccurredAt :: !UTCTime
  , financeTransactionWriteOccurredAtSupplied :: !Bool
  , financeTransactionWriteCounterparty :: !(Maybe String)
  , financeTransactionWriteDescription :: !(Maybe String)
  }

data FinanceTransactionDirection
  = FinanceTransactionSent
  | FinanceTransactionReceived
  deriving (Eq, Show)

data FinanceReportDirection
  = FinanceReportSent
  | FinanceReportReceived
  | FinanceReportAll
  deriving (Eq, Show)

data FinanceReportRequest = FinanceReportRequest
  { financeReportFrom :: !UTCTime
  , financeReportTo :: !UTCTime
  , financeReportDirection :: !FinanceReportDirection
  , financeReportAccountIn :: ![String]
  , financeReportAccountNotIn :: ![String]
  , financeReportCategoryIn :: ![String]
  , financeReportCategoryNotIn :: ![String]
  } deriving (Eq, Show)

data FinanceReportResult = FinanceReportResult
  { financeReportTotal :: !Int
  , financeReportCount :: !Int
  , financeReportTransactionIds :: ![String]
  } deriving (Eq, Show)

data FinanceTransactionSplitRow = FinanceTransactionSplitRow
  { financeTransactionSplitAmount :: !Int
  , financeTransactionSplitCategory :: !String
  } deriving (Eq, Show)

data FinanceTransaction = FinanceTransaction
  { financeTransactionId :: !String
  , financeTransactionDirection :: !FinanceTransactionDirection
  , financeTransactionAccountId :: !String
  , financeTransactionAmount :: !Int
  , financeTransactionOccurredAt :: !UTCTime
  , financeTransactionRecordedAt :: !UTCTime
  , financeTransactionCounterparty :: !(Maybe String)
  , financeTransactionDescription :: !(Maybe String)
  , financeTransactionTransfer :: !(Maybe FinanceTransactionTransfer)
  , financeTransactionCategory :: !(Maybe String)
  , financeTransactionSplits :: ![FinanceTransactionSplitRow]
  , financeTransactionNotes :: ![FinanceTransactionNote]
  , financeTransactionAdjustment :: !(Maybe FinanceTransactionAdjustment)
  } deriving (Eq, Show)

data FinanceTransactionAdjustment = FinanceTransactionAdjustment
  { financeTransactionAdjustmentSnapshotId :: !String
  , financeTransactionAdjustmentSnapshotOccurredAt :: !UTCTime
  , financeTransactionAdjustmentReason :: !(Maybe String)
  , financeTransactionAdjustmentAmount :: !Int
  , financeTransactionAdjustmentDirection :: !FinanceTransactionDirection
  } deriving (Eq, Show)

data FinanceTransactionNote = FinanceTransactionNote
  { financeTransactionNoteId :: !String
  , financeTransactionNoteText :: !String
  , financeTransactionNoteCreatedAt :: !UTCTime
  , financeTransactionNoteUpdatedAt :: !UTCTime
  } deriving (Eq, Show)

data FinanceTransactionTransfer = FinanceTransactionTransfer
  { financeTransactionTransferLinkType :: !String
  , financeTransactionTransferPeerTransactionId :: !String
  , financeTransactionTransferPeerAccountId :: !String
  , financeTransactionTransferPeerAmount :: !Int
  , financeTransactionTransferLinkedAt :: !UTCTime
  } deriving (Eq, Show)

data FinanceCanonicalEventEnvelope = FinanceCanonicalEventEnvelope
  { financeCanonicalEventNumber :: !Int64
  , financeCanonicalEventId :: !String
  , financeCanonicalEventUserId :: !String
  , financeCanonicalEventStreamId :: !String
  , financeCanonicalEventStreamVersion :: !Int64
  , financeCanonicalEventType :: !String
  , financeCanonicalEventVersion :: !Int
  , financeCanonicalEventOccurredAt :: !UTCTime
  , financeCanonicalEventRecordedAt :: !UTCTime
  , financeCanonicalEventIdempotencyKey :: !(Maybe String)
  , financeCanonicalEventPayload :: !Value
  } deriving (Eq, Show)

instance FromJSON FinanceTransactionCreateRequest where
  parseJSON = withObject "FinanceTransactionCreateRequest" $ \value ->
    FinanceTransactionCreateRequest
      <$> value .: "accountId"
      <*> value .: "amount"
      <*> value .:? "occurredAt"
      <*> value .:? "counterparty"
      <*> value .:? "description"

instance FromJSON FinanceTransactionNoteCreateRequest where
  parseJSON = withObject "FinanceTransactionNoteCreateRequest" $ \value ->
    FinanceTransactionNoteCreateRequest . trimWhitespace <$> value .: "text"

instance FromJSON FinanceTransactionNoteUpdateRequest where
  parseJSON = withObject "FinanceTransactionNoteUpdateRequest" $ \value ->
    FinanceTransactionNoteUpdateRequest . trimWhitespace <$> value .: "text"

instance FromJSON FinanceTransactionLinkRequest where
  parseJSON = withObject "FinanceTransactionLinkRequest" $ \value ->
    FinanceTransactionLinkRequest
      <$> value .: "sourceTransactionId"
      <*> value .: "targetTransactionId"
      <*> value .: "linkType"

instance FromJSON FinanceTransactionCategorizeRequest where
  parseJSON = withObject "FinanceTransactionCategorizeRequest" $ \value ->
    FinanceTransactionCategorizeRequest <$> value .: "category"

instance FromJSON FinanceTransactionSplitRequest where
  parseJSON = withObject "FinanceTransactionSplitRequest" $ \value ->
    FinanceTransactionSplitRequest <$> value .: "splits"

instance FromJSON FinanceTransactionSplitWriteRow where
  parseJSON = withObject "FinanceTransactionSplitWriteRow" $ \value ->
    FinanceTransactionSplitWriteRow
      <$> value .: "amount"
      <*> value .: "category"

instance ToJSON FinanceTransactionSplitWriteRow where
  toJSON FinanceTransactionSplitWriteRow { financeTransactionSplitWriteAmount, financeTransactionSplitWriteCategory } =
    object
      [ "amount" .= financeTransactionSplitWriteAmount
      , "category" .= financeTransactionSplitWriteCategory
      ]

instance ToJSON FinanceTransactionSplitRow where
  toJSON FinanceTransactionSplitRow { financeTransactionSplitAmount, financeTransactionSplitCategory } =
    object
      [ "amount" .= financeTransactionSplitAmount
      , "category" .= financeTransactionSplitCategory
      ]

instance ToJSON FinanceTransactionTransfer where
  toJSON FinanceTransactionTransfer
    { financeTransactionTransferLinkType
    , financeTransactionTransferPeerTransactionId
    , financeTransactionTransferPeerAccountId
    , financeTransactionTransferPeerAmount
    , financeTransactionTransferLinkedAt
    } =
      object
        [ "linkType" .= financeTransactionTransferLinkType
        , "peerTransactionId" .= financeTransactionTransferPeerTransactionId
        , "peerAccountId" .= financeTransactionTransferPeerAccountId
        , "peerAmount" .= financeTransactionTransferPeerAmount
        , "linkedAt" .= financeTransactionTransferLinkedAt
        ]

instance ToJSON FinanceTransactionNote where
  toJSON FinanceTransactionNote
    { financeTransactionNoteId
    , financeTransactionNoteText
    , financeTransactionNoteCreatedAt
    , financeTransactionNoteUpdatedAt
    } =
      object
        [ "id" .= financeTransactionNoteId
        , "text" .= financeTransactionNoteText
        , "createdAt" .= financeTransactionNoteCreatedAt
        , "updatedAt" .= financeTransactionNoteUpdatedAt
        ]

instance ToJSON FinanceTransaction where
  toJSON FinanceTransaction
    { financeTransactionId
    , financeTransactionDirection
    , financeTransactionAccountId
    , financeTransactionAmount
    , financeTransactionOccurredAt
    , financeTransactionRecordedAt
    , financeTransactionCounterparty
    , financeTransactionDescription
    , financeTransactionTransfer
    , financeTransactionCategory
    , financeTransactionSplits
    , financeTransactionNotes
    , financeTransactionAdjustment
    } =
      object
        [ "id" .= financeTransactionId
        , "direction" .= financeTransactionDirectionText financeTransactionDirection
        , "accountId" .= financeTransactionAccountId
        , "amount" .= financeTransactionAmount
        , "occurredAt" .= financeTransactionOccurredAt
        , "recordedAt" .= financeTransactionRecordedAt
        , "counterparty" .= financeTransactionCounterparty
        , "description" .= financeTransactionDescription
        , "transfer" .= financeTransactionTransfer
        , "category" .= financeTransactionCategory
        , "splits" .= financeTransactionSplits
        , "notes" .= financeTransactionNotes
        , "adjustment" .= financeTransactionAdjustment
        ]

instance ToJSON FinanceTransactionAdjustment where
  toJSON FinanceTransactionAdjustment
    { financeTransactionAdjustmentSnapshotId
    , financeTransactionAdjustmentSnapshotOccurredAt
    , financeTransactionAdjustmentReason
    , financeTransactionAdjustmentAmount
    , financeTransactionAdjustmentDirection
    } =
      object
        [ "snapshotId" .= financeTransactionAdjustmentSnapshotId
        , "snapshotOccurredAt" .= financeTransactionAdjustmentSnapshotOccurredAt
        , "reason" .= financeTransactionAdjustmentReason
        , "amount" .= financeTransactionAdjustmentAmount
        , "direction" .= financeTransactionDirectionText financeTransactionAdjustmentDirection
        ]

instance ToJSON FinanceReportResult where
  toJSON FinanceReportResult { financeReportTotal, financeReportCount, financeReportTransactionIds } =
    object
      [ "total" .= financeReportTotal
      , "count" .= financeReportCount
      , "transactionIds" .= financeReportTransactionIds
      ]

instance ToJSON FinanceCanonicalEventEnvelope where
  toJSON FinanceCanonicalEventEnvelope
    { financeCanonicalEventNumber
    , financeCanonicalEventId
    , financeCanonicalEventUserId
    , financeCanonicalEventStreamId
    , financeCanonicalEventStreamVersion
    , financeCanonicalEventType
    , financeCanonicalEventVersion
    , financeCanonicalEventOccurredAt
    , financeCanonicalEventRecordedAt
    , financeCanonicalEventIdempotencyKey
    , financeCanonicalEventPayload
    } =
      object
        [ "eventNumber" .= financeCanonicalEventNumber
        , "eventId" .= financeCanonicalEventId
        , "userId" .= financeCanonicalEventUserId
        , "streamId" .= financeCanonicalEventStreamId
        , "streamVersion" .= financeCanonicalEventStreamVersion
        , "eventType" .= financeCanonicalEventType
        , "eventVersion" .= financeCanonicalEventVersion
        , "occurredAt" .= financeCanonicalEventOccurredAt
        , "recordedAt" .= financeCanonicalEventRecordedAt
        , "idempotencyKey" .= financeCanonicalEventIdempotencyKey
        , "payload" .= financeCanonicalEventPayload
        ]

financeTransactionDirectionText :: FinanceTransactionDirection -> Text
financeTransactionDirectionText FinanceTransactionSent = "sent"
financeTransactionDirectionText FinanceTransactionReceived = "received"

postgresFinanceTransactionRepository :: Pool Connection -> FinanceTransactionRepository
postgresFinanceTransactionRepository pool =
  FinanceTransactionRepository
    { repoCreateFinanceTransaction = pgCreateFinanceTransaction pool
    , repoLoadFinanceTransactionById = pgLoadFinanceTransactionById pool
    , repoListFinanceTransactions = pgListFinanceTransactions pool
    , repoGetFinanceReport = pgGetFinanceReport pool
    , repoAddFinanceTransactionNote = pgAddFinanceTransactionNote pool
    , repoUpdateFinanceTransactionNote = pgUpdateFinanceTransactionNote pool
    , repoDeleteFinanceTransactionNote = pgDeleteFinanceTransactionNote pool
    , repoCategorizeFinanceTransaction = pgCategorizeFinanceTransaction pool
    , repoSplitFinanceTransaction = pgSplitFinanceTransaction pool
    , repoLinkFinanceTransactions = pgLinkFinanceTransactions pool
    , repoListFinanceCanonicalEvents = pgListFinanceCanonicalEvents pool
    }

financeTransactionPostgresHealthChecks :: Connection -> ExceptT String IO ()
financeTransactionPostgresHealthChecks conn = do
  tryExcept
    (query_ conn
      "SELECT event_number, event_id, user_id, stream_id, stream_version, event_type, event_version, occurred_at, recorded_at, idempotency_key, payload FROM finance_events LIMIT 0"
      :: IO [(Int64, String, String, String, Int64, String, Int, UTCTime, UTCTime, Maybe String, Value)])
    (\err -> "Finance schema check failed for finance_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions LIMIT 0"
      :: IO [(String, String, String, String, Int64, UTCTime, UTCTime, Maybe String, Maybe String)])
    (\err -> "Finance schema check failed for finance_transactions: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, idempotency_key, direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id, counterparty, description FROM finance_transaction_idempotency LIMIT 0"
      :: IO [(String, String, String, String, Int64, Bool, UTCTime, String, Maybe String, Maybe String)])
    (\err -> "Finance schema check failed for finance_transaction_idempotency: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, category, updated_at FROM finance_transaction_categories LIMIT 0"
      :: IO [(String, String, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_categories: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, split_index, amount, category, updated_at FROM finance_transaction_splits LIMIT 0"
      :: IO [(String, String, Int, Int64, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_splits: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, peer_transaction_id, link_type, linked_at FROM finance_transaction_links LIMIT 0"
      :: IO [(String, String, String, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_links: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, note_id, note_text, created_at, updated_at FROM finance_transaction_notes LIMIT 0"
      :: IO [(String, String, String, String, UTCTime, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_notes: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, account_id, snapshot_id, snapshot_occurred_at, amount, direction, reason, recorded_at FROM finance_balance_snapshot_adjustments LIMIT 0"
      :: IO [(String, String, String, UTCTime, Int64, Text, Maybe String, UTCTime)])
    (\err -> "Finance schema check failed for finance_balance_snapshot_adjustments: " ++ show err)
  pure ()

pgCreateFinanceTransaction :: Pool Connection -> String -> FinanceTransactionWriteRequest -> ExceptT RepositoryError IO FinanceTransaction
pgCreateFinanceTransaction pool userId request@FinanceTransactionWriteRequest
  { financeTransactionWriteIdempotencyKey
  , financeTransactionWriteDirection
  , financeTransactionWriteAccountId
  , financeTransactionWriteAmount
  , financeTransactionWriteOccurredAt
  , financeTransactionWriteOccurredAtSupplied
  , financeTransactionWriteCounterparty
  , financeTransactionWriteDescription
  } =
    withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
      normalizedCounterparty <- normalizeCounterparty financeTransactionWriteCounterparty
      normalizedDescription <- normalizeDescription financeTransactionWriteDescription
      existingRows <- tryExcept
        (query conn
          "SELECT direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id, counterparty, description FROM finance_transaction_idempotency WHERE user_id = ? AND idempotency_key = ?"
          (userId, financeTransactionWriteIdempotencyKey))
        mapSqlReadException
      case existingRows of
        [(storedDirection, storedAccountId, storedAmount, storedOccurredAtSupplied, storedOccurredAt, transactionId, storedCounterparty, storedDescription)] ->
          if idempotencyMatches request normalizedCounterparty normalizedDescription storedDirection storedAccountId storedAmount storedOccurredAtSupplied storedOccurredAt storedCounterparty storedDescription
            then pgLoadFinanceTransactionByIdInConn conn userId transactionId
            else throwError AlreadyExists
        [] -> do
          transactionId <- liftIO (toString <$> nextRandom)
          eventId <- liftIO (toString <$> nextRandom)
          _ <- tryExcept
            (withTransaction conn $ do
              recordedAt <- appendFinanceEvent conn FinanceCanonicalEvent
                { canonicalEventId = eventId
                , canonicalEventUserId = userId
                , canonicalEventStreamId = "transaction:" ++ transactionId
                , canonicalEventType =
                    case financeTransactionWriteDirection of
                      FinanceTransactionSent -> "MoneySent"
                      FinanceTransactionReceived -> "MoneyReceived"
                , canonicalEventOccurredAt = Just financeTransactionWriteOccurredAt
                , canonicalEventIdempotencyKey = Just financeTransactionWriteIdempotencyKey
                , canonicalEventPayload =
                    object
                      [ "transactionId" .= transactionId
                      , "accountId" .= financeTransactionWriteAccountId
                      , "direction" .= financeTransactionDirectionText financeTransactionWriteDirection
                      , "amount" .= financeTransactionWriteAmount
                      , "occurredAtSupplied" .= financeTransactionWriteOccurredAtSupplied
                      , "counterparty" .= normalizedCounterparty
                      , "description" .= normalizedDescription
                      ]
                }
              _ <- execute conn
                "INSERT INTO finance_transactions (user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at, counterparty, description) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                (userId, transactionId, financeTransactionWriteAccountId, financeTransactionDirectionText financeTransactionWriteDirection, financeTransactionWriteAmount, financeTransactionWriteOccurredAt, recordedAt, normalizedCounterparty, normalizedDescription)
              _ <- execute conn
                "INSERT INTO finance_transaction_idempotency (user_id, idempotency_key, direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id, counterparty, description) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                ( userId
                , financeTransactionWriteIdempotencyKey
                , financeTransactionDirectionText financeTransactionWriteDirection
                , financeTransactionWriteAccountId
                , financeTransactionWriteAmount
                , financeTransactionWriteOccurredAtSupplied
                , financeTransactionWriteOccurredAt
                , transactionId
                , normalizedCounterparty
                , normalizedDescription
                )
              pure ())
            mapSqlWriteException
          pgLoadFinanceTransactionByIdInConn conn userId transactionId
        _ -> throwError ReadFailure

pgLoadFinanceTransactionById :: Pool Connection -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgLoadFinanceTransactionById pool userId transactionId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn ->
    pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgListFinanceCanonicalEvents :: Pool Connection -> String -> ExceptT RepositoryError IO [FinanceCanonicalEventEnvelope]
pgListFinanceCanonicalEvents pool userId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept
      (query conn
        "SELECT event_number, event_id, user_id, stream_id, stream_version, event_type, event_version, occurred_at, recorded_at, idempotency_key, payload FROM finance_events WHERE user_id = ? ORDER BY event_number ASC"
        (Only userId)
        :: IO [(Int64, String, String, String, Int64, String, Int, UTCTime, UTCTime, Maybe String, Value)])
      mapSqlReadException
    pure
      [ FinanceCanonicalEventEnvelope
          { financeCanonicalEventNumber = eventNumber
          , financeCanonicalEventId = eventId
          , financeCanonicalEventUserId = rowUserId
          , financeCanonicalEventStreamId = streamId
          , financeCanonicalEventStreamVersion = streamVersion
          , financeCanonicalEventType = eventType
          , financeCanonicalEventVersion = eventVersion
          , financeCanonicalEventOccurredAt = occurredAt
          , financeCanonicalEventRecordedAt = recordedAt
          , financeCanonicalEventIdempotencyKey = idempotencyKey
          , financeCanonicalEventPayload = payload
          }
      | ( eventNumber
        , eventId
        , rowUserId
        , streamId
        , streamVersion
        , eventType
        , eventVersion
        , occurredAt
        , recordedAt
        , idempotencyKey
        , payload
        ) <- rows
      ]

pgLoadFinanceTransactionByIdInConn :: Connection -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgLoadFinanceTransactionByIdInConn conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
      (userId, transactionId)
      :: IO [(String, Text, String, Int64, UTCTime, UTCTime, Maybe String, Maybe String)])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [row] -> do
      base <- decodeBaseTransaction row
      hydrateTransactionDetails conn userId base
    _ -> throwError ReadFailure

pgListFinanceTransactions :: Pool Connection -> String -> Maybe String -> Maybe UTCTime -> Maybe UTCTime -> ExceptT RepositoryError IO [FinanceTransaction]
pgListFinanceTransactions pool userId mAccountId mFrom mTo =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept (runListQuery conn) mapSqlReadException
    baseRows <- mapM decodeBaseTransaction rows
    hydratedBaseRows <- mapM (hydrateTransactionDetails conn userId) baseRows
    rawAdjustmentRows <- tryExcept (runAdjustmentListQuery conn) mapSqlReadException
    adjustmentRows <- mapM decodeAdjustmentListRow rawAdjustmentRows
    let adjustmentTransactions = map financeAdjustmentRowToTransaction adjustmentRows
    pure (sortOn financeTransactionSortKey (hydratedBaseRows ++ adjustmentTransactions))
  where
    runListQuery conn =
      case (mAccountId, mFrom, mTo) of
        (Nothing, Nothing, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? ORDER BY occurred_at DESC, transaction_id ASC"
            (Only userId)
        (Just accountId, Nothing, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND account_id = ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId)
        (Nothing, Just fromTs, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND occurred_at >= ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, fromTs)
        (Nothing, Nothing, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, toTs)
        (Just accountId, Just fromTs, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at >= ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, fromTs)
        (Just accountId, Nothing, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, toTs)
        (Nothing, Just fromTs, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND occurred_at >= ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, fromTs, toTs)
        (Just accountId, Just fromTs, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at, counterparty, description FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at >= ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, fromTs, toTs)
    runAdjustmentListQuery conn =
      case (mAccountId, mFrom, mTo) of
        (Nothing, Nothing, Nothing) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (Only userId)
        (Just accountId, Nothing, Nothing) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? AND account_id = ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (userId, accountId)
        (Nothing, Just fromTs, Nothing) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? AND snapshot_occurred_at >= ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (userId, fromTs)
        (Nothing, Nothing, Just toTs) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? AND snapshot_occurred_at < ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (userId, toTs)
        (Just accountId, Just fromTs, Nothing) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? AND account_id = ? AND snapshot_occurred_at >= ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (userId, accountId, fromTs)
        (Just accountId, Nothing, Just toTs) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? AND account_id = ? AND snapshot_occurred_at < ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (userId, accountId, toTs)
        (Nothing, Just fromTs, Just toTs) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? AND snapshot_occurred_at >= ? AND snapshot_occurred_at < ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (userId, fromTs, toTs)
        (Just accountId, Just fromTs, Just toTs) ->
          query conn
            "SELECT snapshot_id, snapshot_occurred_at, recorded_at, account_id, amount, direction, reason FROM finance_balance_snapshot_adjustments WHERE user_id = ? AND account_id = ? AND snapshot_occurred_at >= ? AND snapshot_occurred_at < ? ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
            (userId, accountId, fromTs, toTs)

pgGetFinanceReport :: Pool Connection -> String -> FinanceReportRequest -> ExceptT RepositoryError IO FinanceReportResult
pgGetFinanceReport pool userId request =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rawBaseRows <- tryExcept (loadBaseRows conn) mapSqlReadException
    rawAdjustmentRows <- tryExcept (loadAdjustmentRows conn) mapSqlReadException
    baseRows <- mapM decodeReportBaseRow rawBaseRows
    adjustmentRows <- mapM decodeAdjustmentReportBaseRow rawAdjustmentRows
    let combinedRows = baseRows ++ adjustmentRows
    if null combinedRows
      then pure FinanceReportResult { financeReportTotal = 0, financeReportCount = 0, financeReportTransactionIds = [] }
      else do
        let transactionIds = map baseRowId combinedRows
        categoryRows <- tryExcept
          (query conn
            "SELECT transaction_id, category FROM finance_transaction_categories WHERE user_id = ? AND transaction_id IN ?"
            (userId, In transactionIds)
            :: IO [(String, String)])
          mapSqlReadException
        splitRows <- tryExcept
          (query conn
            "SELECT transaction_id, split_index, amount, category FROM finance_transaction_splits WHERE user_id = ? AND transaction_id IN ? ORDER BY transaction_id ASC, split_index ASC"
            (userId, In transactionIds)
            :: IO [(String, Int, Int64, String)])
          mapSqlReadException
        let categoryMap = Map.fromList categoryRows
            splitMap = Map.fromListWith (++) [ (txId, [(splitIndex, splitAmount, splitCategory)]) | (txId, splitIndex, splitAmount, splitCategory) <- splitRows ]
            evaluated = map (evaluateReportRow request categoryMap splitMap) combinedRows
            matched = [ row | Just row <- evaluated ]
            total = sum (map matchedContribution matched)
            ids = map matchedId matched
        pure FinanceReportResult
          { financeReportTotal = total
          , financeReportCount = length matched
          , financeReportTransactionIds = ids
          }
  where
    loadBaseRows conn =
      query conn
        "SELECT t.transaction_id, t.direction, t.account_id, t.amount, t.occurred_at \
        \FROM finance_transactions t \
        \LEFT JOIN finance_transaction_links l ON l.user_id = t.user_id AND l.transaction_id = t.transaction_id \
        \WHERE t.user_id = ? \
        \AND t.occurred_at >= ? AND t.occurred_at < ? \
        \AND l.transaction_id IS NULL \
        \AND (? = '' OR t.direction = ?) \
        \ORDER BY t.occurred_at DESC, t.transaction_id ASC"
        ( userId
        , financeReportFrom request
        , financeReportTo request
        , reportDirectionSqlFilter (financeReportDirection request)
        , reportDirectionSqlFilter (financeReportDirection request)
        )
        :: IO [(String, Text, String, Int64, UTCTime)]
    loadAdjustmentRows conn =
      query conn
        "SELECT snapshot_id, snapshot_occurred_at, account_id, amount, direction, reason \
        \FROM finance_balance_snapshot_adjustments \
        \WHERE user_id = ? \
        \AND snapshot_occurred_at >= ? AND snapshot_occurred_at < ? \
        \AND (? = '' OR direction = ?) \
        \ORDER BY snapshot_occurred_at DESC, snapshot_id ASC"
        ( userId
        , financeReportFrom request
        , financeReportTo request
        , reportDirectionSqlFilter (financeReportDirection request)
        , reportDirectionSqlFilter (financeReportDirection request)
        )
        :: IO [(String, UTCTime, String, Int64, Text, Maybe String)]

data ReportBaseRow = ReportBaseRow
  { baseRowId :: !String
  , baseRowDirection :: !FinanceTransactionDirection
  , baseRowAccountId :: !String
  , baseRowAmount :: !Int
  , baseRowIsAdjustment :: !Bool
  , baseRowAdjustmentReason :: !(Maybe String)
  , baseRowAdjustmentSnapshotOccurredAt :: !(Maybe UTCTime)
  , baseRowAdjustmentRecordedAt :: !(Maybe UTCTime)
}

data ReportMatchedRow = ReportMatchedRow
  { matchedId :: !String
  , matchedContribution :: !Int
  }

decodeReportBaseRow :: (String, Text, String, Int64, UTCTime) -> ExceptT RepositoryError IO ReportBaseRow
decodeReportBaseRow (transactionId, directionText, accountId, amount, _occurredAt) =
  case directionFromText directionText of
    Nothing -> throwError ReadFailure
    Just parsedDirection ->
      pure ReportBaseRow
        { baseRowId = transactionId
        , baseRowDirection = parsedDirection
        , baseRowAccountId = accountId
        , baseRowAmount = fromIntegral amount
        , baseRowIsAdjustment = False
        , baseRowAdjustmentReason = Nothing
        , baseRowAdjustmentSnapshotOccurredAt = Nothing
        , baseRowAdjustmentRecordedAt = Nothing
        }

decodeAdjustmentReportBaseRow :: (String, UTCTime, String, Int64, Text, Maybe String) -> ExceptT RepositoryError IO ReportBaseRow
decodeAdjustmentReportBaseRow (snapshotId, snapshotOccurredAt, accountId, amount, directionText, reason) =
  case directionFromText directionText of
    Nothing -> throwError ReadFailure
    Just parsedDirection ->
      pure ReportBaseRow
        { baseRowId = "adjustment:" ++ snapshotId
        , baseRowDirection = parsedDirection
        , baseRowAccountId = accountId
        , baseRowAmount = fromIntegral amount
        , baseRowIsAdjustment = True
        , baseRowAdjustmentReason = reason
        , baseRowAdjustmentSnapshotOccurredAt = Just snapshotOccurredAt
        , baseRowAdjustmentRecordedAt = Nothing
        }

evaluateReportRow
  :: FinanceReportRequest
  -> Map.Map String String
  -> Map.Map String [(Int, Int64, String)]
  -> ReportBaseRow
  -> Maybe ReportMatchedRow
evaluateReportRow request categoryMap splitMap baseRow =
  let categoryInSet = Set.fromList (financeReportCategoryIn request)
      categoryNotInSet = Set.fromList (financeReportCategoryNotIn request)
      accountInSet = Set.fromList (financeReportAccountIn request)
      accountNotInSet = Set.fromList (financeReportAccountNotIn request)
      accountIncluded =
        (Set.null accountInSet || Set.member (baseRowAccountId baseRow) accountInSet)
          && not (Set.member (baseRowAccountId baseRow) accountNotInSet)
      hasCategoryFilters = not (Set.null categoryInSet) || not (Set.null categoryNotInSet)
      orderedSplits =
        if baseRowIsAdjustment baseRow
          then []
          else map (\(_, amount, categorySlug) -> (fromIntegral amount, categorySlug)) $
            maybe [] (sortOn (\(splitIndex, _, _) -> splitIndex)) (Map.lookup (baseRowId baseRow) splitMap)
      signed value =
        case financeReportDirection request of
          FinanceReportAll ->
            case baseRowDirection baseRow of
              FinanceTransactionReceived -> value
              FinanceTransactionSent -> negate value
          _ -> value
      matchesCategory categorySlug =
        let normalizedCategory = if categorySlug `elem` ["uncategorized.expense", "uncategorized.income"] then "uncategorized" else categorySlug
            includeOk = Set.null categoryInSet || Set.member normalizedCategory categoryInSet
            excludeOk = not (Set.member normalizedCategory categoryNotInSet)
         in includeOk && excludeOk
      uncategorizedWholeMatch =
        if baseRowIsAdjustment baseRow
          then True
          else case Map.lookup (baseRowId baseRow) categoryMap of
            Nothing -> True
            Just categorySlug -> categorySlug `elem` ["uncategorized.expense", "uncategorized.income"]
      wholeCategorySlug =
        if baseRowIsAdjustment baseRow
          then "uncategorized"
          else case Map.lookup (baseRowId baseRow) categoryMap of
            Nothing -> "uncategorized"
            Just categorySlug
              | categorySlug `elem` ["uncategorized.expense", "uncategorized.income"] -> "uncategorized"
              | otherwise -> categorySlug
   in if not accountIncluded
        then Nothing
        else if hasCategoryFilters
        then
          if not (null orderedSplits)
            then
              let matchingSplitAmounts = [ amount | (amount, categorySlug) <- orderedSplits, matchesCategory categorySlug ]
               in if null matchingSplitAmounts
                    then Nothing
                    else Just ReportMatchedRow { matchedId = baseRowId baseRow, matchedContribution = signed (sum matchingSplitAmounts) }
            else
              let normalizedWhole = if uncategorizedWholeMatch then "uncategorized" else wholeCategorySlug
               in if matchesCategory normalizedWhole
                    then Just ReportMatchedRow { matchedId = baseRowId baseRow, matchedContribution = signed (baseRowAmount baseRow) }
                    else Nothing
        else
          Just ReportMatchedRow { matchedId = baseRowId baseRow, matchedContribution = signed (baseRowAmount baseRow) }

reportDirectionSqlFilter :: FinanceReportDirection -> Text
reportDirectionSqlFilter FinanceReportAll = ""
reportDirectionSqlFilter FinanceReportSent = "sent"
reportDirectionSqlFilter FinanceReportReceived = "received"

pgAddFinanceTransactionNote :: Pool Connection -> String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgAddFinanceTransactionNote pool userId transactionId noteText =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    case normalizeNoteText noteText of
      Nothing -> throwError WriteFailure
      Just normalizedText -> do
        _ <- requireTransactionAmount conn userId transactionId
        eventId <- liftIO (toString <$> nextRandom)
        noteId <- liftIO (toString <$> nextRandom)
        _ <- tryExcept
          (withTransaction conn $ do
            recordedAt <- appendFinanceEvent conn FinanceCanonicalEvent
              { canonicalEventId = eventId
              , canonicalEventUserId = userId
              , canonicalEventStreamId = "transaction:" ++ transactionId
              , canonicalEventType = "TransactionNoteAdded"
              , canonicalEventOccurredAt = Nothing
              , canonicalEventIdempotencyKey = Nothing
              , canonicalEventPayload =
                  object
                    [ "transactionId" .= transactionId
                    , "noteId" .= noteId
                    , "noteText" .= normalizedText
                    ]
              }
            _ <- execute conn
              "INSERT INTO finance_transaction_notes (user_id, transaction_id, note_id, note_text, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?)"
              (userId, transactionId, noteId, normalizedText, recordedAt, recordedAt)
            pure ())
          mapSqlWriteException
        pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgUpdateFinanceTransactionNote :: Pool Connection -> String -> String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgUpdateFinanceTransactionNote pool userId transactionId noteId noteText =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    case normalizeNoteText noteText of
      Nothing -> throwError WriteFailure
      Just normalizedText -> do
        _ <- requireTransactionAmount conn userId transactionId
        _ <- requireTransactionNote conn userId transactionId noteId
        eventId <- liftIO (toString <$> nextRandom)
        _ <- tryExcept
          (withTransaction conn $ do
            recordedAt <- appendFinanceEvent conn FinanceCanonicalEvent
              { canonicalEventId = eventId
              , canonicalEventUserId = userId
              , canonicalEventStreamId = "transaction:" ++ transactionId
              , canonicalEventType = "TransactionNoteUpdated"
              , canonicalEventOccurredAt = Nothing
              , canonicalEventIdempotencyKey = Nothing
              , canonicalEventPayload =
                  object
                    [ "transactionId" .= transactionId
                    , "noteId" .= noteId
                    , "noteText" .= normalizedText
                    ]
              }
            updatedRows <- execute conn
              "UPDATE finance_transaction_notes SET note_text = ?, updated_at = ? WHERE user_id = ? AND transaction_id = ? AND note_id = ?"
              (normalizedText, recordedAt, userId, transactionId, noteId)
            if updatedRows == 1
              then pure ()
              else fail "Unexpected updated row count for finance transaction note update"
          )
          mapSqlWriteException
        pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgDeleteFinanceTransactionNote :: Pool Connection -> String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgDeleteFinanceTransactionNote pool userId transactionId noteId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireTransactionAmount conn userId transactionId
    _ <- requireTransactionNote conn userId transactionId noteId
    eventId <- liftIO (toString <$> nextRandom)
    _ <- tryExcept
      (withTransaction conn $ do
        _ <- appendFinanceEvent conn FinanceCanonicalEvent
          { canonicalEventId = eventId
          , canonicalEventUserId = userId
          , canonicalEventStreamId = "transaction:" ++ transactionId
          , canonicalEventType = "TransactionNoteDeleted"
          , canonicalEventOccurredAt = Nothing
          , canonicalEventIdempotencyKey = Nothing
          , canonicalEventPayload =
              object
                [ "transactionId" .= transactionId
                , "noteId" .= noteId
                ]
          }
        deletedRows <- execute conn
          "DELETE FROM finance_transaction_notes WHERE user_id = ? AND transaction_id = ? AND note_id = ?"
          (userId, transactionId, noteId)
        if deletedRows == 1
          then pure ()
          else fail "Unexpected deleted row count for finance transaction note delete"
      )
      mapSqlWriteException
    pgLoadFinanceTransactionByIdInConn conn userId transactionId

normalizeNoteText :: String -> Maybe String
normalizeNoteText text =
  let trimmed = trimWhitespace text
   in if null trimmed || length trimmed > 2000
        then Nothing
        else Just trimmed

normalizeCounterparty :: Maybe String -> ExceptT RepositoryError IO (Maybe String)
normalizeCounterparty mCounterparty =
  case fmap (map toLower . trimWhitespace) mCounterparty of
    Nothing -> pure Nothing
    Just value
      | null value -> pure Nothing
      | length value > 120 -> throwError WriteFailure
      | otherwise -> pure (Just value)

normalizeDescription :: Maybe String -> ExceptT RepositoryError IO (Maybe String)
normalizeDescription mDescription =
  case fmap trimWhitespace mDescription of
    Nothing -> pure Nothing
    Just value
      | null value -> pure Nothing
      | length value > 1000 -> throwError WriteFailure
      | otherwise -> pure (Just value)

trimWhitespace :: String -> String
trimWhitespace = dropWhile isSpace . dropWhileEnd isSpace

requireTransactionNote :: Connection -> String -> String -> String -> ExceptT RepositoryError IO ()
requireTransactionNote conn userId transactionId noteId = do
  rows <- tryExcept
    (query conn
      "SELECT note_id FROM finance_transaction_notes WHERE user_id = ? AND transaction_id = ? AND note_id = ? LIMIT 1"
      (userId, transactionId, noteId)
      :: IO [Only String])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [_] -> pure ()
    _ -> throwError ReadFailure

pgCategorizeFinanceTransaction :: Pool Connection -> String -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgCategorizeFinanceTransaction pool userId transactionId categorySlug =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireTransactionAmount conn userId transactionId
    validateCategorySlug conn userId categorySlug
    activeSplitRows <- tryExcept
      (query conn
        "SELECT split_index FROM finance_transaction_splits WHERE user_id = ? AND transaction_id = ? LIMIT 1"
        (userId, transactionId)
        :: IO [Only Int])
      mapSqlReadException
    if not (null activeSplitRows)
      then throwError AlreadyExists
      else do
        eventId <- liftIO (toString <$> nextRandom)
        _ <- tryExcept
          (withTransaction conn $ do
            _ <- appendFinanceEvent conn FinanceCanonicalEvent
              { canonicalEventId = eventId
              , canonicalEventUserId = userId
              , canonicalEventStreamId = "transaction:" ++ transactionId
              , canonicalEventType = "TransactionCategorized"
              , canonicalEventOccurredAt = Nothing
              , canonicalEventIdempotencyKey = Nothing
              , canonicalEventPayload =
                  object
                    [ "transactionId" .= transactionId
                    , "category" .= categorySlug
                    ]
              }
            _ <- execute conn
              "DELETE FROM finance_transaction_categories WHERE user_id = ? AND transaction_id = ?"
              (userId, transactionId)
            _ <- execute conn
              "INSERT INTO finance_transaction_categories (user_id, transaction_id, category) VALUES (?, ?, ?)"
              (userId, transactionId, categorySlug)
            pure ())
          mapSqlWriteException
        pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgSplitFinanceTransaction :: Pool Connection -> String -> String -> [FinanceTransactionSplitWriteRow] -> ExceptT RepositoryError IO FinanceTransaction
pgSplitFinanceTransaction pool userId transactionId splitRows =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    transactionAmount <- requireTransactionAmount conn userId transactionId
    validateSplitRows conn userId transactionAmount splitRows
    eventId <- liftIO (toString <$> nextRandom)
    _ <- tryExcept
      (withTransaction conn $ do
        _ <- appendFinanceEvent conn FinanceCanonicalEvent
          { canonicalEventId = eventId
          , canonicalEventUserId = userId
          , canonicalEventStreamId = "transaction:" ++ transactionId
          , canonicalEventType = "TransactionSplit"
          , canonicalEventOccurredAt = Nothing
          , canonicalEventIdempotencyKey = Nothing
          , canonicalEventPayload =
              object
                [ "transactionId" .= transactionId
                , "splits" .= splitRows
                ]
          }
        _ <- execute conn
          "DELETE FROM finance_transaction_categories WHERE user_id = ? AND transaction_id = ?"
          (userId, transactionId)
        _ <- execute conn
          "DELETE FROM finance_transaction_splits WHERE user_id = ? AND transaction_id = ?"
          (userId, transactionId)
        insertSplitRows conn userId transactionId splitRows
        pure ())
      mapSqlWriteException
    pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgLinkFinanceTransactions :: Pool Connection -> String -> String -> String -> String -> ExceptT RepositoryError IO (FinanceTransaction, FinanceTransaction)
pgLinkFinanceTransactions pool userId sourceTransactionId targetTransactionId linkType =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    if linkType /= "transfer"
      then throwError WriteFailure
      else do
        source <- loadTransactionFacts conn userId sourceTransactionId
        target <- loadTransactionFacts conn userId targetTransactionId
        validateTransferPair source target
        sourceLinked <- transactionHasLink conn userId sourceTransactionId
        targetLinked <- transactionHasLink conn userId targetTransactionId
        if sourceLinked || targetLinked
          then throwError AlreadyExists
          else do
            eventId <- liftIO (toString <$> nextRandom)
            _ <- tryExcept
              (withTransaction conn $ do
                _ <- appendFinanceEvent conn FinanceCanonicalEvent
                  { canonicalEventId = eventId
                  , canonicalEventUserId = userId
                  , canonicalEventStreamId = "transaction:" ++ sourceTransactionId
                  , canonicalEventType = "TransactionLinked"
                  , canonicalEventOccurredAt = Nothing
                  , canonicalEventIdempotencyKey = Nothing
                  , canonicalEventPayload =
                      object
                        [ "sourceTransactionId" .= sourceTransactionId
                        , "targetTransactionId" .= targetTransactionId
                        , "linkType" .= linkType
                        ]
                  }
                _ <- execute conn
                  "INSERT INTO finance_transaction_links (user_id, transaction_id, peer_transaction_id, link_type) VALUES (?, ?, ?, ?)"
                  (userId, sourceTransactionId, targetTransactionId, linkType)
                _ <- execute conn
                  "INSERT INTO finance_transaction_links (user_id, transaction_id, peer_transaction_id, link_type) VALUES (?, ?, ?, ?)"
                  (userId, targetTransactionId, sourceTransactionId, linkType)
                pure ())
              mapSqlWriteException
            sourceTransaction <- pgLoadFinanceTransactionByIdInConn conn userId sourceTransactionId
            targetTransaction <- pgLoadFinanceTransactionByIdInConn conn userId targetTransactionId
            pure (sourceTransaction, targetTransaction)

data TransactionFacts = TransactionFacts
  { transactionFactsId :: !String
  , transactionFactsDirection :: !FinanceTransactionDirection
  , transactionFactsAccountId :: !String
  , transactionFactsAmount :: !Int
  }

loadTransactionFacts :: Connection -> String -> String -> ExceptT RepositoryError IO TransactionFacts
loadTransactionFacts conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT transaction_id, direction, account_id, amount FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
      (userId, transactionId)
      :: IO [(String, Text, String, Int64)])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [(loadedId, directionText, accountId, amount)] ->
      case directionFromText directionText of
        Nothing -> throwError ReadFailure
        Just parsedDirection ->
          pure TransactionFacts
            { transactionFactsId = loadedId
            , transactionFactsDirection = parsedDirection
            , transactionFactsAccountId = accountId
            , transactionFactsAmount = fromIntegral amount
            }
    _ -> throwError ReadFailure

transactionHasLink :: Connection -> String -> String -> ExceptT RepositoryError IO Bool
transactionHasLink conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT transaction_id FROM finance_transaction_links WHERE user_id = ? AND transaction_id = ? LIMIT 1"
      (userId, transactionId)
      :: IO [Only String])
    mapSqlReadException
  pure (not (null rows))

validateTransferPair :: TransactionFacts -> TransactionFacts -> ExceptT RepositoryError IO ()
validateTransferPair source target
  | transactionFactsId source == transactionFactsId target = throwError WriteFailure
  | transactionFactsAccountId source == transactionFactsAccountId target = throwError WriteFailure
  | transactionFactsDirection source == transactionFactsDirection target = throwError WriteFailure
  | transactionFactsAmount source /= transactionFactsAmount target = throwError WriteFailure
  | otherwise = pure ()

insertSplitRows :: Connection -> String -> String -> [FinanceTransactionSplitWriteRow] -> IO ()
insertSplitRows _ _ _ [] = pure ()
insertSplitRows conn userId transactionId rows =
  mapM_ insertOne (zip [0 :: Int ..] rows)
  where
    insertOne (splitIndex, FinanceTransactionSplitWriteRow amount categorySlug) = do
      _ <- execute conn
        "INSERT INTO finance_transaction_splits (user_id, transaction_id, split_index, amount, category) VALUES (?, ?, ?, ?, ?)"
        (userId, transactionId, splitIndex, amount, categorySlug)
      pure ()

requireTransactionAmount :: Connection -> String -> String -> ExceptT RepositoryError IO Int
requireTransactionAmount conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT amount FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
      (userId, transactionId)
      :: IO [Only Int64])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [Only amount] -> pure (fromIntegral amount)
    _ -> throwError ReadFailure

validateSplitRows :: Connection -> String -> Int -> [FinanceTransactionSplitWriteRow] -> ExceptT RepositoryError IO ()
validateSplitRows conn userId transactionAmount splitRows = do
  if length splitRows < 2
    then throwError WriteFailure
    else pure ()
  let amounts = map financeTransactionSplitWriteAmount splitRows
  if any (<= 0) amounts
    then throwError WriteFailure
    else pure ()
  if sum amounts /= transactionAmount
    then throwError WriteFailure
    else pure ()
  mapM_ (validateCategorySlug conn userId . financeTransactionSplitWriteCategory) splitRows

validateCategorySlug :: Connection -> String -> String -> ExceptT RepositoryError IO ()
validateCategorySlug conn userId categorySlug = do
  rows <- tryExcept
    (query conn
      "SELECT selectable FROM finance_categories WHERE category_id = ? AND (user_id IS NULL OR user_id = ?)"
      (categorySlug, userId)
      :: IO [Only Bool])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [Only selectable] ->
      if selectable
        then pure ()
        else throwError WriteFailure
    _ -> throwError ReadFailure

hydrateTransactionDetails :: Connection -> String -> FinanceTransaction -> ExceptT RepositoryError IO FinanceTransaction
hydrateTransactionDetails conn userId transaction = do
  withTransfer <- hydrateTransferState conn userId transaction
  withClassification <- hydrateClassificationState conn userId withTransfer
  hydrateNotesState conn userId withClassification

hydrateTransferState :: Connection -> String -> FinanceTransaction -> ExceptT RepositoryError IO FinanceTransaction
hydrateTransferState conn userId transaction = do
  rows <- tryExcept
    (query conn
      "SELECT peer_transaction_id, link_type, linked_at FROM finance_transaction_links WHERE user_id = ? AND transaction_id = ?"
      (userId, financeTransactionId transaction)
      :: IO [(String, String, UTCTime)])
    mapSqlReadException
  case rows of
    [] -> pure transaction { financeTransactionTransfer = Nothing }
    [(peerTransactionId, linkType, linkedAt)] -> do
      peerRows <- tryExcept
        (query conn
          "SELECT account_id, amount FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
          (userId, peerTransactionId)
          :: IO [(String, Int64)])
        mapSqlReadException
      case peerRows of
        [(peerAccountId, peerAmount)] ->
          pure transaction
            { financeTransactionTransfer =
                Just FinanceTransactionTransfer
                  { financeTransactionTransferLinkType = linkType
                  , financeTransactionTransferPeerTransactionId = peerTransactionId
                  , financeTransactionTransferPeerAccountId = peerAccountId
                  , financeTransactionTransferPeerAmount = fromIntegral peerAmount
                  , financeTransactionTransferLinkedAt = linkedAt
                  }
            }
        [] -> throwError ReadFailure
        _ -> throwError ReadFailure
    _ -> throwError ReadFailure

hydrateClassificationState :: Connection -> String -> FinanceTransaction -> ExceptT RepositoryError IO FinanceTransaction
hydrateClassificationState conn userId transaction = do
  splitRows <- tryExcept
    (query conn
      "SELECT amount, category FROM finance_transaction_splits WHERE user_id = ? AND transaction_id = ? ORDER BY split_index ASC"
      (userId, financeTransactionId transaction)
      :: IO [(Int64, String)])
    mapSqlReadException
  if not (null splitRows)
    then do
      let decodedSplits =
            [ FinanceTransactionSplitRow
                { financeTransactionSplitAmount = fromIntegral amount
                , financeTransactionSplitCategory = categorySlug
                }
            | (amount, categorySlug) <- splitRows
            ]
      pure transaction
        { financeTransactionCategory = Nothing
        , financeTransactionSplits = decodedSplits
        }
    else do
      categoryRows <- tryExcept
        (query conn
          "SELECT category FROM finance_transaction_categories WHERE user_id = ? AND transaction_id = ?"
          (userId, financeTransactionId transaction)
          :: IO [Only String])
        mapSqlReadException
      case categoryRows of
        [] -> pure transaction { financeTransactionCategory = Nothing, financeTransactionSplits = [] }
        [Only categorySlug] -> pure transaction { financeTransactionCategory = Just categorySlug, financeTransactionSplits = [] }
        _ -> throwError ReadFailure

hydrateNotesState :: Connection -> String -> FinanceTransaction -> ExceptT RepositoryError IO FinanceTransaction
hydrateNotesState conn userId transaction = do
  rows <- tryExcept
    (query conn
      "SELECT note_id, note_text, created_at, updated_at FROM finance_transaction_notes WHERE user_id = ? AND transaction_id = ? ORDER BY created_at ASC, note_id ASC"
      (userId, financeTransactionId transaction)
      :: IO [(String, String, UTCTime, UTCTime)])
    mapSqlReadException
  let notes =
        [ FinanceTransactionNote
            { financeTransactionNoteId = noteId
            , financeTransactionNoteText = noteText
            , financeTransactionNoteCreatedAt = createdAt
            , financeTransactionNoteUpdatedAt = updatedAt
            }
        | (noteId, noteText, createdAt, updatedAt) <- rows
        ]
  pure transaction { financeTransactionNotes = notes }

decodeBaseTransaction :: (String, Text, String, Int64, UTCTime, UTCTime, Maybe String, Maybe String) -> ExceptT RepositoryError IO FinanceTransaction
decodeBaseTransaction (transactionId, directionText, accountId, amount, occurredAt, recordedAt, counterparty, description) =
  case directionFromText directionText of
    Nothing -> throwError ReadFailure
    Just financeTransactionDirection ->
      pure FinanceTransaction
        { financeTransactionId = transactionId
        , financeTransactionDirection = financeTransactionDirection
        , financeTransactionAccountId = accountId
        , financeTransactionAmount = fromIntegral amount
        , financeTransactionOccurredAt = occurredAt
        , financeTransactionRecordedAt = recordedAt
        , financeTransactionCounterparty = counterparty
        , financeTransactionDescription = description
        , financeTransactionTransfer = Nothing
        , financeTransactionCategory = Nothing
        , financeTransactionSplits = []
        , financeTransactionNotes = []
        , financeTransactionAdjustment = Nothing
        }

data FinanceAdjustmentRow = FinanceAdjustmentRow
  { financeAdjustmentRowSnapshotId :: !String
  , financeAdjustmentRowSnapshotOccurredAt :: !UTCTime
  , financeAdjustmentRowRecordedAt :: !UTCTime
  , financeAdjustmentRowAccountId :: !String
  , financeAdjustmentRowAmount :: !Int
  , financeAdjustmentRowDirection :: !FinanceTransactionDirection
  , financeAdjustmentRowReason :: !(Maybe String)
  }

decodeAdjustmentListRow :: (String, UTCTime, UTCTime, String, Int64, Text, Maybe String) -> ExceptT RepositoryError IO FinanceAdjustmentRow
decodeAdjustmentListRow (snapshotId, snapshotOccurredAt, recordedAt, accountId, amount, directionText, reason) =
  case directionFromText directionText of
    Nothing -> throwError ReadFailure
    Just parsedDirection ->
      pure FinanceAdjustmentRow
        { financeAdjustmentRowSnapshotId = snapshotId
        , financeAdjustmentRowSnapshotOccurredAt = snapshotOccurredAt
        , financeAdjustmentRowRecordedAt = recordedAt
        , financeAdjustmentRowAccountId = accountId
        , financeAdjustmentRowAmount = fromIntegral amount
        , financeAdjustmentRowDirection = parsedDirection
        , financeAdjustmentRowReason = reason
        }

financeAdjustmentRowToTransaction :: FinanceAdjustmentRow -> FinanceTransaction
financeAdjustmentRowToTransaction FinanceAdjustmentRow
  { financeAdjustmentRowSnapshotId
  , financeAdjustmentRowSnapshotOccurredAt
  , financeAdjustmentRowRecordedAt
  , financeAdjustmentRowAccountId
  , financeAdjustmentRowAmount
  , financeAdjustmentRowDirection
  , financeAdjustmentRowReason
  } =
    FinanceTransaction
      { financeTransactionId = "adjustment:" ++ financeAdjustmentRowSnapshotId
      , financeTransactionDirection = financeAdjustmentRowDirection
      , financeTransactionAccountId = financeAdjustmentRowAccountId
      , financeTransactionAmount = financeAdjustmentRowAmount
      , financeTransactionOccurredAt = financeAdjustmentRowSnapshotOccurredAt
      , financeTransactionRecordedAt = financeAdjustmentRowRecordedAt
      , financeTransactionCounterparty = Nothing
      , financeTransactionDescription = Nothing
      , financeTransactionTransfer = Nothing
      , financeTransactionCategory = Nothing
      , financeTransactionSplits = []
      , financeTransactionNotes = []
      , financeTransactionAdjustment =
          Just FinanceTransactionAdjustment
            { financeTransactionAdjustmentSnapshotId = financeAdjustmentRowSnapshotId
            , financeTransactionAdjustmentSnapshotOccurredAt = financeAdjustmentRowSnapshotOccurredAt
            , financeTransactionAdjustmentReason = financeAdjustmentRowReason
            , financeTransactionAdjustmentAmount = financeAdjustmentRowAmount
            , financeTransactionAdjustmentDirection = financeAdjustmentRowDirection
            }
      }

financeTransactionSortKey :: FinanceTransaction -> (Down UTCTime, String)
financeTransactionSortKey transaction =
  (Down (financeTransactionOccurredAt transaction), financeTransactionId transaction)

idempotencyMatches :: FinanceTransactionWriteRequest -> Maybe String -> Maybe String -> Text -> String -> Int64 -> Bool -> UTCTime -> Maybe String -> Maybe String -> Bool
idempotencyMatches FinanceTransactionWriteRequest
  { financeTransactionWriteDirection
  , financeTransactionWriteAccountId
  , financeTransactionWriteAmount
  , financeTransactionWriteOccurredAt
  , financeTransactionWriteOccurredAtSupplied
  }
  normalizedCounterparty
  normalizedDescription
  storedDirection
  storedAccountId
  storedAmount
  storedOccurredAtSupplied
  storedOccurredAt
  storedCounterparty
  storedDescription =
    financeTransactionDirectionText financeTransactionWriteDirection == storedDirection
      && financeTransactionWriteAccountId == storedAccountId
      && fromIntegral financeTransactionWriteAmount == storedAmount
      && financeTransactionWriteOccurredAtSupplied == storedOccurredAtSupplied
      && (not financeTransactionWriteOccurredAtSupplied || financeTransactionWriteOccurredAt == storedOccurredAt)
      && normalizedCounterparty == storedCounterparty
      && normalizedDescription == storedDescription

data FinanceCanonicalEvent = FinanceCanonicalEvent
  { canonicalEventId :: !String
  , canonicalEventUserId :: !String
  , canonicalEventStreamId :: !String
  , canonicalEventType :: !String
  , canonicalEventOccurredAt :: !(Maybe UTCTime)
  , canonicalEventIdempotencyKey :: !(Maybe String)
  , canonicalEventPayload :: !Value
  }

appendFinanceEvent :: Connection -> FinanceCanonicalEvent -> IO UTCTime
appendFinanceEvent conn FinanceCanonicalEvent
  { canonicalEventId
  , canonicalEventUserId
  , canonicalEventStreamId
  , canonicalEventType
  , canonicalEventOccurredAt
  , canonicalEventIdempotencyKey
  , canonicalEventPayload
  } = do
    lockRows <- query conn
      "SELECT pg_try_advisory_xact_lock(hashtext(?), 0)"
      (Only canonicalEventStreamId)
      :: IO [Only Bool]
    case lockRows of
      [Only True] -> pure ()
      [Only False] -> fail "Unable to acquire stream advisory lock for finance event append"
      _ -> fail "Unexpected advisory lock query result for finance event append"
    streamVersionRows <- query conn
      "SELECT COALESCE(MAX(stream_version), 0)::bigint + 1 FROM finance_events WHERE stream_id = ?"
      (Only canonicalEventStreamId)
      :: IO [Only Int64]
    streamVersion <- case streamVersionRows of
      [Only value] -> pure value
      _ -> fail "Unable to compute next stream version for finance event append"
    occurredAt <- case canonicalEventOccurredAt of
      Just value -> pure value
      Nothing -> queryNow conn
    recordedRows <- query conn
      "INSERT INTO finance_events (event_id, user_id, stream_id, stream_version, event_type, event_version, occurred_at, idempotency_key, payload) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING recorded_at"
      ( canonicalEventId
      , canonicalEventUserId
      , canonicalEventStreamId
      , streamVersion
      , canonicalEventType
      , (1 :: Int)
      , occurredAt
      , canonicalEventIdempotencyKey
      , canonicalEventPayload
      )
      :: IO [Only UTCTime]
    case recordedRows of
      [Only recordedAt] -> pure recordedAt
      _ -> fail "Unexpected recorded_at row count for finance event insert"

queryNow :: Connection -> IO UTCTime
queryNow conn = do
  rows <- query_ conn "SELECT NOW()" :: IO [Only UTCTime]
  case rows of
    [Only value] -> pure value
    _ -> fail "Unable to resolve current timestamp"

directionFromText :: Text -> Maybe FinanceTransactionDirection
directionFromText "sent" = Just FinanceTransactionSent
directionFromText "received" = Just FinanceTransactionReceived
directionFromText _ = Nothing
