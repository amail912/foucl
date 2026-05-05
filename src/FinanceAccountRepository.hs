{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FinanceAccountRepository
  ( FinanceAccountRepository(..)
  , FinanceAccount(..)
  , FinanceAccountCreateRequest(..)
  , FinanceAccountSnapshotCreateRequest(..)
  , FinanceAccountSnapshot(..)
  , FinanceAccountReconciliation(..)
  , FinanceAccountStatus(..)
  , FinanceAccountStatusFilter(..)
  , financeAccountStatusText
  , parseFinanceAccountStatusFilter
  , normalizeFinanceAccountName
  , parseFinanceAccountStatus
  , postgresFinanceAccountRepository
  , financeAccountPostgresHealthChecks
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), object, withObject, (.:), (.=))
import Data.Char (isSpace, toLower)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , execute
  , query
  , query_
  , withTransaction
  )
import Helpers (mapSqlReadException, mapSqlWriteException, tryExcept, withPoolExceptHandled)
import Repository (RepositoryError(..))

data FinanceAccountRepository = FinanceAccountRepository
  { repoCreateFinanceAccount :: !(String -> String -> ExceptT RepositoryError IO FinanceAccount)
  , repoListFinanceAccounts :: !(String -> FinanceAccountStatusFilter -> ExceptT RepositoryError IO [FinanceAccount])
  , repoGetFinanceAccountById :: !(String -> String -> ExceptT RepositoryError IO FinanceAccount)
  , repoCloseFinanceAccount :: !(String -> String -> ExceptT RepositoryError IO FinanceAccount)
  , repoCreateFinanceAccountSnapshot :: !(String -> String -> Int -> UTCTime -> ExceptT RepositoryError IO FinanceAccountReconciliation)
  , repoListFinanceAccountSnapshots :: !(String -> String -> ExceptT RepositoryError IO [FinanceAccountSnapshot])
  , repoGetFinanceAccountReconciliationLatest :: !(String -> String -> ExceptT RepositoryError IO FinanceAccountReconciliation)
  , repoGetFinanceAccountReconciliationBySnapshotId :: !(String -> String -> String -> ExceptT RepositoryError IO FinanceAccountReconciliation)
  }

data FinanceAccountCreateRequest = FinanceAccountCreateRequest
  { financeAccountCreateName :: !String
  }

data FinanceAccountSnapshotCreateRequest = FinanceAccountSnapshotCreateRequest
  { financeAccountSnapshotCreateBalance :: !Int
  , financeAccountSnapshotCreateOccurredAt :: !String
  }

data FinanceAccountStatus
  = FinanceAccountActive
  | FinanceAccountClosed
  deriving (Eq, Show)

data FinanceAccountStatusFilter
  = FinanceAccountsActive
  | FinanceAccountsClosed
  | FinanceAccountsAll
  deriving (Eq, Show)

data FinanceAccount = FinanceAccount
  { financeAccountId :: !String
  , financeAccountName :: !String
  , financeAccountStatus :: !FinanceAccountStatus
  } deriving (Eq, Show)

data FinanceAccountSnapshot = FinanceAccountSnapshot
  { financeAccountSnapshotId :: !String
  , financeAccountSnapshotOccurredAt :: !UTCTime
  , financeAccountSnapshotBalance :: !Int
  } deriving (Eq, Show)

data FinanceAccountReconciliation = FinanceAccountReconciliation
  { financeAccountReconciliationSnapshotId :: !String
  , financeAccountReconciliationSnapshotOccurredAt :: !UTCTime
  , financeAccountReconciliationObservedBalance :: !Int
  , financeAccountReconciliationDerivedBalanceAtSnapshot :: !Int
  , financeAccountReconciliationDiscrepancy :: !Int
  } deriving (Eq, Show)

instance FromJSON FinanceAccountCreateRequest where
  parseJSON = withObject "FinanceAccountCreateRequest" $ \value ->
    FinanceAccountCreateRequest <$> value .: "name"

instance FromJSON FinanceAccountSnapshotCreateRequest where
  parseJSON = withObject "FinanceAccountSnapshotCreateRequest" $ \value ->
    FinanceAccountSnapshotCreateRequest
      <$> value .: "balance"
      <*> value .: "occurredAt"

instance ToJSON FinanceAccount where
  toJSON FinanceAccount { financeAccountId, financeAccountName, financeAccountStatus } =
    object
      [ "id" .= financeAccountId
      , "name" .= financeAccountName
      , "status" .= financeAccountStatusText financeAccountStatus
      ]

instance ToJSON FinanceAccountSnapshot where
  toJSON FinanceAccountSnapshot
    { financeAccountSnapshotId
    , financeAccountSnapshotOccurredAt
    , financeAccountSnapshotBalance
    } =
      object
        [ "id" .= financeAccountSnapshotId
        , "occurredAt" .= financeAccountSnapshotOccurredAt
        , "balance" .= financeAccountSnapshotBalance
        ]

instance ToJSON FinanceAccountReconciliation where
  toJSON FinanceAccountReconciliation
    { financeAccountReconciliationSnapshotId
    , financeAccountReconciliationSnapshotOccurredAt
    , financeAccountReconciliationObservedBalance
    , financeAccountReconciliationDerivedBalanceAtSnapshot
    , financeAccountReconciliationDiscrepancy
    } =
      object
        [ "snapshotId" .= financeAccountReconciliationSnapshotId
        , "snapshotOccurredAt" .= financeAccountReconciliationSnapshotOccurredAt
        , "observedBalance" .= financeAccountReconciliationObservedBalance
        , "derivedBalanceAtSnapshot" .= financeAccountReconciliationDerivedBalanceAtSnapshot
        , "discrepancy" .= financeAccountReconciliationDiscrepancy
        ]

financeAccountStatusText :: FinanceAccountStatus -> Text
financeAccountStatusText FinanceAccountActive = "active"
financeAccountStatusText FinanceAccountClosed = "closed"

parseFinanceAccountStatusFilter :: Maybe String -> Maybe FinanceAccountStatusFilter
parseFinanceAccountStatusFilter Nothing = Just FinanceAccountsActive
parseFinanceAccountStatusFilter (Just "active") = Just FinanceAccountsActive
parseFinanceAccountStatusFilter (Just "closed") = Just FinanceAccountsClosed
parseFinanceAccountStatusFilter (Just "all") = Just FinanceAccountsAll
parseFinanceAccountStatusFilter _ = Nothing

normalizeFinanceAccountName :: String -> Maybe String
normalizeFinanceAccountName raw =
  let trimmed = trim raw
  in if null trimmed then Nothing else Just trimmed

postgresFinanceAccountRepository :: Pool Connection -> FinanceAccountRepository
postgresFinanceAccountRepository pool =
  FinanceAccountRepository
    { repoCreateFinanceAccount = pgCreateFinanceAccount pool
    , repoListFinanceAccounts = pgListFinanceAccounts pool
    , repoGetFinanceAccountById = pgGetFinanceAccountById pool
    , repoCloseFinanceAccount = pgCloseFinanceAccount pool
    , repoCreateFinanceAccountSnapshot = pgCreateFinanceAccountSnapshot pool
    , repoListFinanceAccountSnapshots = pgListFinanceAccountSnapshots pool
    , repoGetFinanceAccountReconciliationLatest = pgGetFinanceAccountReconciliationLatest pool
    , repoGetFinanceAccountReconciliationBySnapshotId = pgGetFinanceAccountReconciliationBySnapshotId pool
    }

financeAccountPostgresHealthChecks :: Connection -> ExceptT String IO ()
financeAccountPostgresHealthChecks conn = do
  tryExcept
    (query_ conn
      "SELECT event_id, user_id, account_id, event_type, display_name, normalized_name, status, recorded_at FROM finance_account_events LIMIT 0"
      :: IO [(String, String, String, String, String, String, String, String)])
    (\err -> "Finance schema check failed for finance_account_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, account_id, display_name, normalized_name, status, opened_at FROM finance_accounts LIMIT 0"
      :: IO [(String, String, String, String, String, String)])
    (\err -> "Finance schema check failed for finance_accounts: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT event_id, user_id, account_id, snapshot_id, event_type, balance, occurred_at, recorded_at FROM finance_balance_snapshot_events LIMIT 0"
      :: IO [(String, String, String, String, String, Int64, UTCTime, UTCTime)])
    (\err -> "Finance schema check failed for finance_balance_snapshot_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, account_id, snapshot_id, balance, occurred_at, recorded_at FROM finance_balance_snapshots LIMIT 0"
      :: IO [(String, String, String, Int64, UTCTime, UTCTime)])
    (\err -> "Finance schema check failed for finance_balance_snapshots: " ++ show err)
  pure ()

pgCreateFinanceAccount :: Pool Connection -> String -> String -> ExceptT RepositoryError IO FinanceAccount
pgCreateFinanceAccount pool userId displayName =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    accountId <- liftIO (toString <$> nextRandom)
    eventId <- liftIO (toString <$> nextRandom)
    let normalizedName = map toLower displayName
        statusText = financeAccountStatusText FinanceAccountActive
    _ <- tryExcept
      (withTransaction conn $ do
        _ <- execute conn
          "INSERT INTO finance_account_events (event_id, user_id, account_id, event_type, display_name, normalized_name, status) VALUES (?, ?, ?, ?, ?, ?, ?)"
          (eventId, userId, accountId, ("AccountOpened" :: String), displayName, normalizedName, statusText)
        _ <- execute conn
          "INSERT INTO finance_accounts (user_id, account_id, display_name, normalized_name, status) VALUES (?, ?, ?, ?, ?)"
          (userId, accountId, displayName, normalizedName, statusText)
        pure ())
      mapSqlWriteException
    pure FinanceAccount
      { financeAccountId = accountId
      , financeAccountName = displayName
      , financeAccountStatus = FinanceAccountActive
      }

pgListFinanceAccounts :: Pool Connection -> String -> FinanceAccountStatusFilter -> ExceptT RepositoryError IO [FinanceAccount]
pgListFinanceAccounts pool userId statusFilter =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept (runQuery conn) mapSqlReadException
    mapM decodeFinanceAccount rows
  where
    runQuery conn =
      case statusFilter of
        FinanceAccountsActive ->
          query conn
            "SELECT account_id, display_name, status FROM finance_accounts WHERE user_id = ? AND status = 'active' ORDER BY display_name, account_id"
            (Only userId)
        FinanceAccountsClosed ->
          query conn
            "SELECT account_id, display_name, status FROM finance_accounts WHERE user_id = ? AND status = 'closed' ORDER BY display_name, account_id"
            (Only userId)
        FinanceAccountsAll ->
          query conn
            "SELECT account_id, display_name, status FROM finance_accounts WHERE user_id = ? ORDER BY display_name, account_id"
            (Only userId)

pgGetFinanceAccountById :: Pool Connection -> String -> String -> ExceptT RepositoryError IO FinanceAccount
pgGetFinanceAccountById pool userId accountId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept
      (query conn
        "SELECT account_id, display_name, status FROM finance_accounts WHERE user_id = ? AND account_id = ?"
        (userId, accountId))
      mapSqlReadException
    case rows of
      [] -> throwError NotFound
      [row] -> decodeFinanceAccount row
      _ -> throwError ReadFailure

pgCloseFinanceAccount :: Pool Connection -> String -> String -> ExceptT RepositoryError IO FinanceAccount
pgCloseFinanceAccount pool userId accountId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    eventId <- liftIO (toString <$> nextRandom)
    result <- tryExcept
      (withTransaction conn $ do
        rows <- query conn
          "SELECT account_id, display_name, normalized_name, status FROM finance_accounts WHERE user_id = ? AND account_id = ? FOR UPDATE"
          (userId, accountId)
          :: IO [(String, String, String, Text)]
        case rows of
          [] -> pure Nothing
          [(rowAccountId, displayName, normalizedName, statusText)] ->
            if statusText == financeAccountStatusText FinanceAccountClosed
              then pure (Just (rowAccountId, displayName, statusText))
              else do
                _ <- execute conn
                  "INSERT INTO finance_account_events (event_id, user_id, account_id, event_type, display_name, normalized_name, status) VALUES (?, ?, ?, ?, ?, ?, ?)"
                  (eventId, userId, accountId, ("AccountClosed" :: String), displayName, normalizedName, financeAccountStatusText FinanceAccountClosed)
                _ <- execute conn
                  "UPDATE finance_accounts SET status = ? WHERE user_id = ? AND account_id = ?"
                  (financeAccountStatusText FinanceAccountClosed, userId, accountId)
                pure (Just (rowAccountId, displayName, financeAccountStatusText FinanceAccountClosed))
          _ -> pure Nothing)
      mapSqlWriteException
    case result of
      Nothing -> throwError NotFound
      Just (rowAccountId, displayName, statusText) ->
        case parseFinanceAccountStatus statusText of
          Nothing -> throwError ReadFailure
          Just financeAccountStatus ->
            pure FinanceAccount
              { financeAccountId = rowAccountId
              , financeAccountName = displayName
              , financeAccountStatus = financeAccountStatus
              }

pgCreateFinanceAccountSnapshot :: Pool Connection -> String -> String -> Int -> UTCTime -> ExceptT RepositoryError IO FinanceAccountReconciliation
pgCreateFinanceAccountSnapshot pool userId accountId observedBalance occurredAt =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireAccountExists conn userId accountId
    eventId <- liftIO (toString <$> nextRandom)
    snapshotId <- liftIO (toString <$> nextRandom)
    _ <- tryExcept
      (withTransaction conn $ do
        recordedRows <- query conn
          "INSERT INTO finance_balance_snapshot_events (event_id, user_id, account_id, snapshot_id, event_type, balance, occurred_at) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING recorded_at"
          (eventId, userId, accountId, snapshotId, ("BalanceSnapshotRecorded" :: String), observedBalance, occurredAt)
          :: IO [Only UTCTime]
        recordedAt <- case recordedRows of
          [Only rowRecordedAt] -> pure rowRecordedAt
          _ -> fail "Unexpected recorded_at row count for finance snapshot event insert"
        _ <- execute conn
          "INSERT INTO finance_balance_snapshots (user_id, account_id, snapshot_id, balance, occurred_at, recorded_at) VALUES (?, ?, ?, ?, ?, ?)"
          (userId, accountId, snapshotId, observedBalance, occurredAt, recordedAt)
        pure ())
      mapSqlWriteException
    computeReconciliation conn userId accountId snapshotId

pgListFinanceAccountSnapshots :: Pool Connection -> String -> String -> ExceptT RepositoryError IO [FinanceAccountSnapshot]
pgListFinanceAccountSnapshots pool userId accountId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireAccountExists conn userId accountId
    rows <- tryExcept
      (query conn
        "SELECT snapshot_id, occurred_at, balance FROM finance_balance_snapshots WHERE user_id = ? AND account_id = ? ORDER BY occurred_at DESC, snapshot_id ASC"
        (userId, accountId)
        :: IO [(String, UTCTime, Int64)])
      mapSqlReadException
    pure
      [ FinanceAccountSnapshot
          { financeAccountSnapshotId = snapshotId
          , financeAccountSnapshotOccurredAt = snapshotOccurredAt
          , financeAccountSnapshotBalance = fromIntegral snapshotBalance
          }
      | (snapshotId, snapshotOccurredAt, snapshotBalance) <- rows
      ]

pgGetFinanceAccountReconciliationLatest :: Pool Connection -> String -> String -> ExceptT RepositoryError IO FinanceAccountReconciliation
pgGetFinanceAccountReconciliationLatest pool userId accountId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireAccountExists conn userId accountId
    rows <- tryExcept
      (query conn
        "SELECT snapshot_id FROM finance_balance_snapshots WHERE user_id = ? AND account_id = ? ORDER BY occurred_at DESC, snapshot_id ASC LIMIT 1"
        (userId, accountId)
        :: IO [Only String])
      mapSqlReadException
    snapshotId <- case rows of
      [Only latestSnapshotId] -> pure latestSnapshotId
      [] -> throwError NotFound
      _ -> throwError ReadFailure
    computeReconciliation conn userId accountId snapshotId

pgGetFinanceAccountReconciliationBySnapshotId :: Pool Connection -> String -> String -> String -> ExceptT RepositoryError IO FinanceAccountReconciliation
pgGetFinanceAccountReconciliationBySnapshotId pool userId accountId snapshotId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireAccountExists conn userId accountId
    computeReconciliation conn userId accountId snapshotId

requireAccountExists :: Connection -> String -> String -> ExceptT RepositoryError IO ()
requireAccountExists conn userId accountId = do
  rows <- tryExcept
    (query conn
      "SELECT account_id FROM finance_accounts WHERE user_id = ? AND account_id = ? LIMIT 1"
      (userId, accountId)
      :: IO [Only String])
    mapSqlReadException
  case rows of
    [] -> throwError NotFound
    [_] -> pure ()
    _ -> throwError ReadFailure

computeReconciliation :: Connection -> String -> String -> String -> ExceptT RepositoryError IO FinanceAccountReconciliation
computeReconciliation conn userId accountId snapshotId = do
  snapshotRows <- tryExcept
    (query conn
      "SELECT occurred_at, balance FROM finance_balance_snapshots WHERE user_id = ? AND account_id = ? AND snapshot_id = ?"
      (userId, accountId, snapshotId)
      :: IO [(UTCTime, Int64)])
    mapSqlReadException
  (snapshotOccurredAt, observedBalance) <- case snapshotRows of
    [(rowOccurredAt, rowBalance)] -> pure (rowOccurredAt, rowBalance)
    [] -> throwError NotFound
    _ -> throwError ReadFailure
  sumRows <- tryExcept
    (query conn
      "SELECT COALESCE(SUM(CASE direction WHEN 'received' THEN amount WHEN 'sent' THEN -amount ELSE 0 END), 0)::bigint FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at <= ?"
      (userId, accountId, snapshotOccurredAt)
      :: IO [Only Int64])
    mapSqlReadException
  derivedBalance <- case sumRows of
    [Only total] -> pure total
    _ -> throwError ReadFailure
  let observedInt = fromIntegral observedBalance
      derivedInt = fromIntegral derivedBalance
  pure FinanceAccountReconciliation
    { financeAccountReconciliationSnapshotId = snapshotId
    , financeAccountReconciliationSnapshotOccurredAt = snapshotOccurredAt
    , financeAccountReconciliationObservedBalance = observedInt
    , financeAccountReconciliationDerivedBalanceAtSnapshot = derivedInt
    , financeAccountReconciliationDiscrepancy = observedInt - derivedInt
    }

decodeFinanceAccount :: (String, String, Text) -> ExceptT RepositoryError IO FinanceAccount
decodeFinanceAccount (accountId, displayName, statusText) =
  case parseFinanceAccountStatus statusText of
    Nothing -> throwError ReadFailure
    Just financeAccountStatus ->
      pure FinanceAccount
        { financeAccountId = accountId
        , financeAccountName = displayName
        , financeAccountStatus = financeAccountStatus
        }

parseFinanceAccountStatus :: Text -> Maybe FinanceAccountStatus
parseFinanceAccountStatus "active" = Just FinanceAccountActive
parseFinanceAccountStatus "closed" = Just FinanceAccountClosed
parseFinanceAccountStatus _ = Nothing

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack
