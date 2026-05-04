{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FinanceTransactionRepository
  ( FinanceTransactionRepository(..)
  , FinanceTransaction(..)
  , FinanceTransactionCreateRequest(..)
  , FinanceTransactionWriteRequest(..)
  , FinanceTransactionDirection(..)
  , financeTransactionDirectionText
  , postgresFinanceTransactionRepository
  , financeTransactionPostgresHealthChecks
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value, object, withObject, (.:), (.:?), (.=))
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , execute
  , query
  , query_
  , Only(..)
  , withTransaction
  )
import Helpers (mapSqlReadException, mapSqlWriteException, tryExcept, withPoolExceptHandled)
import Repository (RepositoryError(..))

data FinanceTransactionRepository = FinanceTransactionRepository
  { repoCreateFinanceTransaction :: !(String -> FinanceTransactionWriteRequest -> ExceptT RepositoryError IO FinanceTransaction)
  , repoLoadFinanceTransactionById :: !(String -> String -> ExceptT RepositoryError IO FinanceTransaction)
  , repoListFinanceTransactions :: !(String -> Maybe String -> Maybe UTCTime -> Maybe UTCTime -> ExceptT RepositoryError IO [FinanceTransaction])
  }

data FinanceTransactionCreateRequest = FinanceTransactionCreateRequest
  { financeTransactionCreateAccountId :: !String
  , financeTransactionCreateAmount :: !Int
  , financeTransactionCreateOccurredAt :: !(Maybe String)
  }

data FinanceTransactionWriteRequest = FinanceTransactionWriteRequest
  { financeTransactionWriteIdempotencyKey :: !String
  , financeTransactionWriteDirection :: !FinanceTransactionDirection
  , financeTransactionWriteAccountId :: !String
  , financeTransactionWriteAmount :: !Int
  , financeTransactionWriteOccurredAt :: !UTCTime
  , financeTransactionWriteOccurredAtSupplied :: !Bool
  }

data FinanceTransactionDirection
  = FinanceTransactionSent
  | FinanceTransactionReceived
  deriving (Eq, Show)

data FinanceTransaction = FinanceTransaction
  { financeTransactionId :: !String
  , financeTransactionDirection :: !FinanceTransactionDirection
  , financeTransactionAccountId :: !String
  , financeTransactionAmount :: !Int
  , financeTransactionOccurredAt :: !UTCTime
  , financeTransactionRecordedAt :: !UTCTime
  } deriving (Eq, Show)

instance FromJSON FinanceTransactionCreateRequest where
  parseJSON = withObject "FinanceTransactionCreateRequest" $ \value ->
    FinanceTransactionCreateRequest
      <$> value .: "accountId"
      <*> value .: "amount"
      <*> value .:? "occurredAt"

instance ToJSON FinanceTransaction where
  toJSON FinanceTransaction
    { financeTransactionId
    , financeTransactionDirection
    , financeTransactionAccountId
    , financeTransactionAmount
    , financeTransactionOccurredAt
    , financeTransactionRecordedAt
    } =
      object
        [ "id" .= financeTransactionId
        , "direction" .= financeTransactionDirectionText financeTransactionDirection
        , "accountId" .= financeTransactionAccountId
        , "amount" .= financeTransactionAmount
        , "occurredAt" .= financeTransactionOccurredAt
        , "recordedAt" .= financeTransactionRecordedAt
        , "transfer" .= (Nothing :: Maybe Value)
        , "category" .= (Nothing :: Maybe Value)
        , "splits" .= ([] :: [Value])
        , "notes" .= ([] :: [Value])
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
    }

financeTransactionPostgresHealthChecks :: Connection -> ExceptT String IO ()
financeTransactionPostgresHealthChecks conn = do
  tryExcept
    (query_ conn
      "SELECT event_id, user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at FROM finance_transaction_events LIMIT 0"
      :: IO [(String, String, String, String, String, Int64, UTCTime, UTCTime)])
    (\err -> "Finance schema check failed for finance_transaction_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at FROM finance_transactions LIMIT 0"
      :: IO [(String, String, String, String, Int64, UTCTime, UTCTime)])
    (\err -> "Finance schema check failed for finance_transactions: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, idempotency_key, direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id FROM finance_transaction_idempotency LIMIT 0"
      :: IO [(String, String, String, String, Int64, Bool, UTCTime, String)])
    (\err -> "Finance schema check failed for finance_transaction_idempotency: " ++ show err)
  pure ()

pgCreateFinanceTransaction :: Pool Connection -> String -> FinanceTransactionWriteRequest -> ExceptT RepositoryError IO FinanceTransaction
pgCreateFinanceTransaction pool userId request@FinanceTransactionWriteRequest
  { financeTransactionWriteIdempotencyKey
  , financeTransactionWriteDirection
  , financeTransactionWriteAccountId
  , financeTransactionWriteAmount
  , financeTransactionWriteOccurredAt
  , financeTransactionWriteOccurredAtSupplied
  } =
    withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
      existingRows <- tryExcept
        (query conn
          "SELECT direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id FROM finance_transaction_idempotency WHERE user_id = ? AND idempotency_key = ?"
          (userId, financeTransactionWriteIdempotencyKey))
        mapSqlReadException
      case existingRows of
        [(storedDirection, storedAccountId, storedAmount, storedOccurredAtSupplied, storedOccurredAt, transactionId)] ->
          if idempotencyMatches request storedDirection storedAccountId storedAmount storedOccurredAtSupplied storedOccurredAt
            then pgLoadFinanceTransactionByIdInConn conn userId transactionId
            else throwError AlreadyExists
        [] -> do
          transactionId <- liftIO (toString <$> nextRandom)
          eventId <- liftIO (toString <$> nextRandom)
          recordedAt <- tryExcept
            (withTransaction conn $ do
              recordedRows <- query conn
                "INSERT INTO finance_transaction_events (event_id, user_id, transaction_id, account_id, direction, amount, occurred_at) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING recorded_at"
                (eventId, userId, transactionId, financeTransactionWriteAccountId, financeTransactionDirectionText financeTransactionWriteDirection, financeTransactionWriteAmount, financeTransactionWriteOccurredAt)
              recordedAt <- case recordedRows of
                [Only rowRecordedAt] -> pure rowRecordedAt
                _ -> fail "Unexpected recorded_at row count for finance transaction event insert"
              _ <- execute conn
                "INSERT INTO finance_transactions (user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
                (userId, transactionId, financeTransactionWriteAccountId, financeTransactionDirectionText financeTransactionWriteDirection, financeTransactionWriteAmount, financeTransactionWriteOccurredAt, recordedAt)
              _ <- execute conn
                "INSERT INTO finance_transaction_idempotency (user_id, idempotency_key, direction, account_id, amount, occurred_at_supplied, occurred_at, transaction_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                ( userId
                , financeTransactionWriteIdempotencyKey
                , financeTransactionDirectionText financeTransactionWriteDirection
                , financeTransactionWriteAccountId
                , financeTransactionWriteAmount
                , financeTransactionWriteOccurredAtSupplied
                , financeTransactionWriteOccurredAt
                , transactionId
                )
              pure recordedAt)
            mapSqlWriteException
          pure FinanceTransaction
            { financeTransactionId = transactionId
            , financeTransactionDirection = financeTransactionWriteDirection
            , financeTransactionAccountId = financeTransactionWriteAccountId
            , financeTransactionAmount = financeTransactionWriteAmount
            , financeTransactionOccurredAt = financeTransactionWriteOccurredAt
            , financeTransactionRecordedAt = recordedAt
            }
        _ -> throwError ReadFailure

pgLoadFinanceTransactionById :: Pool Connection -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgLoadFinanceTransactionById pool userId transactionId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn ->
    pgLoadFinanceTransactionByIdInConn conn userId transactionId

pgLoadFinanceTransactionByIdInConn :: Connection -> String -> String -> ExceptT RepositoryError IO FinanceTransaction
pgLoadFinanceTransactionByIdInConn conn userId transactionId = do
  rows <- tryExcept
    (query conn
      "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND transaction_id = ?"
      (userId, transactionId))
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [row] -> decodeFinanceTransaction row
    _ -> throwError ReadFailure

pgListFinanceTransactions :: Pool Connection -> String -> Maybe String -> Maybe UTCTime -> Maybe UTCTime -> ExceptT RepositoryError IO [FinanceTransaction]
pgListFinanceTransactions pool userId mAccountId mFrom mTo =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept (runListQuery conn) mapSqlReadException
    mapM decodeFinanceTransaction rows
  where
    runListQuery conn =
      case (mAccountId, mFrom, mTo) of
        (Nothing, Nothing, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? ORDER BY occurred_at DESC, transaction_id ASC"
            (Only userId)
        (Just accountId, Nothing, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId)
        (Nothing, Just fromTs, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND occurred_at >= ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, fromTs)
        (Nothing, Nothing, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, toTs)
        (Just accountId, Just fromTs, Nothing) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at >= ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, fromTs)
        (Just accountId, Nothing, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, toTs)
        (Nothing, Just fromTs, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND occurred_at >= ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, fromTs, toTs)
        (Just accountId, Just fromTs, Just toTs) ->
          query conn
            "SELECT transaction_id, direction, account_id, amount, occurred_at, recorded_at FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at >= ? AND occurred_at < ? ORDER BY occurred_at DESC, transaction_id ASC"
            (userId, accountId, fromTs, toTs)

idempotencyMatches :: FinanceTransactionWriteRequest -> Text -> String -> Int64 -> Bool -> UTCTime -> Bool
idempotencyMatches FinanceTransactionWriteRequest
  { financeTransactionWriteDirection
  , financeTransactionWriteAccountId
  , financeTransactionWriteAmount
  , financeTransactionWriteOccurredAt
  , financeTransactionWriteOccurredAtSupplied
  }
  storedDirection
  storedAccountId
  storedAmount
  storedOccurredAtSupplied
  storedOccurredAt =
    financeTransactionDirectionText financeTransactionWriteDirection == storedDirection
      && financeTransactionWriteAccountId == storedAccountId
      && fromIntegral financeTransactionWriteAmount == storedAmount
      && financeTransactionWriteOccurredAtSupplied == storedOccurredAtSupplied
      && (not financeTransactionWriteOccurredAtSupplied || financeTransactionWriteOccurredAt == storedOccurredAt)

decodeFinanceTransaction :: (String, Text, String, Int64, UTCTime, UTCTime) -> ExceptT RepositoryError IO FinanceTransaction
decodeFinanceTransaction (transactionId, directionText, accountId, amount, occurredAt, recordedAt) =
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
        }

directionFromText :: Text -> Maybe FinanceTransactionDirection
directionFromText "sent" = Just FinanceTransactionSent
directionFromText "received" = Just FinanceTransactionReceived
directionFromText _ = Nothing
