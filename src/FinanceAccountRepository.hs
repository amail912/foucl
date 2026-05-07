{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FinanceAccountRepository
  ( FinanceAccountRepository(..)
  , FinanceAccount(..)
  , FinanceAccountCreateRequest(..)
  , FinanceAccountSnapshotCreateRequest(..)
  , FinanceAccountSnapshot(..)
  , FinanceAccountSnapshotView(..)
  , FinanceAccountReconciliation(..)
  , FinanceAccountSnapshotReconciliationStatus(..)
  , FinanceAccountStatus(..)
  , FinanceAccountStatusFilter(..)
  , financeAccountStatusText
  , financeAccountSnapshotReconciliationStatusText
  , parseFinanceAccountStatusFilter
  , normalizeFinanceAccountName
  , parseFinanceAccountStatus
  , parseFinanceAccountSnapshotReconciliationStatus
  , postgresFinanceAccountRepository
  , financeAccountPostgresHealthChecks
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value, object, withObject, (.:), (.=))
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
  , repoSetFinanceAccountSnapshotReconciliationStatus :: !(String -> String -> String -> FinanceAccountSnapshotReconciliationStatus -> ExceptT RepositoryError IO FinanceAccountReconciliation)
  , repoListFinanceAccountSnapshots :: !(String -> String -> ExceptT RepositoryError IO [FinanceAccountSnapshot])
  , repoListFinanceAccountSnapshotsView :: !(String -> ExceptT RepositoryError IO [FinanceAccountSnapshotView])
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
  , financeAccountSnapshotReconciliationStatus :: !FinanceAccountSnapshotReconciliationStatus
  } deriving (Eq, Show)

data FinanceAccountSnapshotView = FinanceAccountSnapshotView
  { financeAccountSnapshotViewAccountId :: !String
  , financeAccountSnapshotViewId :: !String
  , financeAccountSnapshotViewOccurredAt :: !UTCTime
  , financeAccountSnapshotViewBalance :: !Int
  , financeAccountSnapshotViewReconciliationStatus :: !FinanceAccountSnapshotReconciliationStatus
  } deriving (Eq, Show)

data FinanceAccountReconciliation = FinanceAccountReconciliation
  { financeAccountReconciliationSnapshotId :: !String
  , financeAccountReconciliationSnapshotOccurredAt :: !UTCTime
  , financeAccountReconciliationReconciliationStatus :: !FinanceAccountSnapshotReconciliationStatus
  , financeAccountReconciliationObservedBalance :: !Int
  , financeAccountReconciliationDerivedBalanceAtSnapshot :: !Int
  , financeAccountReconciliationDiscrepancy :: !Int
  , financeAccountReconciliationBasisSnapshotId :: !(Maybe String)
  , financeAccountReconciliationBasisSnapshotOccurredAt :: !(Maybe UTCTime)
  } deriving (Eq, Show)

data FinanceAccountSnapshotReconciliationStatus
  = FinanceAccountSnapshotUnreconciled
  | FinanceAccountSnapshotReconciled
  deriving (Eq, Show)

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
    , financeAccountSnapshotReconciliationStatus
    } =
      object
        [ "id" .= financeAccountSnapshotId
        , "occurredAt" .= financeAccountSnapshotOccurredAt
        , "balance" .= financeAccountSnapshotBalance
        , "reconciliationStatus" .= financeAccountSnapshotReconciliationStatusText financeAccountSnapshotReconciliationStatus
        ]

instance ToJSON FinanceAccountSnapshotView where
  toJSON FinanceAccountSnapshotView
    { financeAccountSnapshotViewAccountId
    , financeAccountSnapshotViewId
    , financeAccountSnapshotViewOccurredAt
    , financeAccountSnapshotViewBalance
    , financeAccountSnapshotViewReconciliationStatus
    } =
      object
        [ "accountId" .= financeAccountSnapshotViewAccountId
        , "id" .= financeAccountSnapshotViewId
        , "occurredAt" .= financeAccountSnapshotViewOccurredAt
        , "balance" .= financeAccountSnapshotViewBalance
        , "reconciliationStatus" .= financeAccountSnapshotReconciliationStatusText financeAccountSnapshotViewReconciliationStatus
        ]

instance ToJSON FinanceAccountReconciliation where
  toJSON FinanceAccountReconciliation
    { financeAccountReconciliationSnapshotId
    , financeAccountReconciliationSnapshotOccurredAt
    , financeAccountReconciliationReconciliationStatus
    , financeAccountReconciliationObservedBalance
    , financeAccountReconciliationDerivedBalanceAtSnapshot
    , financeAccountReconciliationDiscrepancy
    , financeAccountReconciliationBasisSnapshotId
    , financeAccountReconciliationBasisSnapshotOccurredAt
    } =
      object
        [ "snapshotId" .= financeAccountReconciliationSnapshotId
        , "snapshotOccurredAt" .= financeAccountReconciliationSnapshotOccurredAt
        , "reconciliationStatus" .= financeAccountSnapshotReconciliationStatusText financeAccountReconciliationReconciliationStatus
        , "observedBalance" .= financeAccountReconciliationObservedBalance
        , "derivedBalanceAtSnapshot" .= financeAccountReconciliationDerivedBalanceAtSnapshot
        , "discrepancy" .= financeAccountReconciliationDiscrepancy
        , "basisSnapshotId" .= financeAccountReconciliationBasisSnapshotId
        , "basisSnapshotOccurredAt" .= financeAccountReconciliationBasisSnapshotOccurredAt
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
    , repoSetFinanceAccountSnapshotReconciliationStatus = pgSetFinanceAccountSnapshotReconciliationStatus pool
    , repoListFinanceAccountSnapshots = pgListFinanceAccountSnapshots pool
    , repoListFinanceAccountSnapshotsView = pgListFinanceAccountSnapshotsView pool
    , repoGetFinanceAccountReconciliationLatest = pgGetFinanceAccountReconciliationLatest pool
    , repoGetFinanceAccountReconciliationBySnapshotId = pgGetFinanceAccountReconciliationBySnapshotId pool
    }

financeAccountPostgresHealthChecks :: Connection -> ExceptT String IO ()
financeAccountPostgresHealthChecks conn = do
  tryExcept
    (query_ conn
      "SELECT event_number, event_id, user_id, stream_id, stream_version, event_type, event_version, occurred_at, recorded_at, idempotency_key, payload FROM finance_events LIMIT 0"
      :: IO [(Int64, String, String, String, Int64, String, Int, UTCTime, UTCTime, Maybe String, Value)])
    (\err -> "Finance schema check failed for finance_events: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, account_id, display_name, normalized_name, status, opened_at FROM finance_accounts LIMIT 0"
      :: IO [(String, String, String, String, String, UTCTime)])
    (\err -> "Finance schema check failed for finance_accounts: " ++ show err)
  tryExcept
    (query_ conn
      "SELECT user_id, account_id, snapshot_id, balance, occurred_at, recorded_at, reconciliation_status FROM finance_balance_snapshots LIMIT 0"
      :: IO [(String, String, String, Int64, UTCTime, UTCTime, Text)])
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
        _ <- appendFinanceEvent conn FinanceCanonicalEvent
          { canonicalEventId = eventId
          , canonicalEventUserId = userId
          , canonicalEventStreamId = "account:" ++ accountId
          , canonicalEventType = "AccountOpened"
          , canonicalEventOccurredAt = Nothing
          , canonicalEventIdempotencyKey = Nothing
          , canonicalEventPayload =
              object
                [ "accountId" .= accountId
                , "displayName" .= displayName
                , "normalizedName" .= normalizedName
                , "status" .= statusText
                ]
          }
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
                _ <- appendFinanceEvent conn FinanceCanonicalEvent
                  { canonicalEventId = eventId
                  , canonicalEventUserId = userId
                  , canonicalEventStreamId = "account:" ++ accountId
                  , canonicalEventType = "AccountClosed"
                  , canonicalEventOccurredAt = Nothing
                  , canonicalEventIdempotencyKey = Nothing
                  , canonicalEventPayload =
                      object
                        [ "accountId" .= accountId
                        , "displayName" .= displayName
                        , "normalizedName" .= normalizedName
                        , "status" .= financeAccountStatusText FinanceAccountClosed
                        ]
                  }
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
        recordedAt <- appendFinanceEvent conn FinanceCanonicalEvent
          { canonicalEventId = eventId
          , canonicalEventUserId = userId
          , canonicalEventStreamId = "account:" ++ accountId
          , canonicalEventType = "BalanceSnapshotRecorded"
          , canonicalEventOccurredAt = Just occurredAt
          , canonicalEventIdempotencyKey = Nothing
          , canonicalEventPayload =
              object
                [ "accountId" .= accountId
                , "snapshotId" .= snapshotId
                , "balance" .= observedBalance
                ]
          }
        _ <- execute conn
          "INSERT INTO finance_balance_snapshots (user_id, account_id, snapshot_id, balance, occurred_at, recorded_at, reconciliation_status) VALUES (?, ?, ?, ?, ?, ?, ?)"
          (userId, accountId, snapshotId, observedBalance, occurredAt, recordedAt, financeAccountSnapshotReconciliationStatusText FinanceAccountSnapshotUnreconciled)
        pure ())
      mapSqlWriteException
    computeReconciliation conn userId accountId snapshotId

pgSetFinanceAccountSnapshotReconciliationStatus :: Pool Connection -> String -> String -> String -> FinanceAccountSnapshotReconciliationStatus -> ExceptT RepositoryError IO FinanceAccountReconciliation
pgSetFinanceAccountSnapshotReconciliationStatus pool userId accountId snapshotId status =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    result <- tryExcept
      (withTransaction conn $ do
        rows <- query conn
          "SELECT occurred_at, balance, reconciliation_status FROM finance_balance_snapshots WHERE user_id = ? AND account_id = ? AND snapshot_id = ? FOR UPDATE"
          (userId, accountId, snapshotId)
          :: IO [(UTCTime, Int64, Text)]
        case rows of
          [] -> pure False
          [(_, _, currentStatusText)] ->
            case parseFinanceAccountSnapshotReconciliationStatus currentStatusText of
              Nothing -> fail "Unexpected finance snapshot reconciliation status in storage"
              Just currentStatus ->
                if currentStatus == status
                  then pure True
                  else do
                    eventId <- toString <$> nextRandom
                    _ <- appendFinanceEvent conn FinanceCanonicalEvent
                      { canonicalEventId = eventId
                      , canonicalEventUserId = userId
                      , canonicalEventStreamId = "snapshot:" ++ snapshotId
                      , canonicalEventType = "BalanceSnapshotReconciliationStatusSet"
                      , canonicalEventOccurredAt = Nothing
                      , canonicalEventIdempotencyKey = Nothing
                      , canonicalEventPayload =
                          object
                            [ "accountId" .= accountId
                            , "snapshotId" .= snapshotId
                            , "status" .= financeAccountSnapshotReconciliationStatusText status
                            ]
                      }
                    updatedRows <- execute conn
                      "UPDATE finance_balance_snapshots SET reconciliation_status = ? WHERE user_id = ? AND account_id = ? AND snapshot_id = ?"
                      (financeAccountSnapshotReconciliationStatusText status, userId, accountId, snapshotId)
                    if updatedRows == 1
                      then pure True
                      else fail "Unexpected updated row count for finance snapshot reconciliation status change")
      mapSqlReadException
    case result of
      False -> throwError NotFound
      True -> computeReconciliation conn userId accountId snapshotId

pgListFinanceAccountSnapshots :: Pool Connection -> String -> String -> ExceptT RepositoryError IO [FinanceAccountSnapshot]
pgListFinanceAccountSnapshots pool userId accountId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    _ <- requireAccountExists conn userId accountId
    rows <- tryExcept
      (query conn
        "SELECT snapshot_id, occurred_at, balance, reconciliation_status FROM finance_balance_snapshots WHERE user_id = ? AND account_id = ? ORDER BY occurred_at DESC, snapshot_id ASC"
        (userId, accountId)
        :: IO [(String, UTCTime, Int64, Text)])
      mapSqlReadException
    mapM decodeSnapshotRow rows
  where
    decodeSnapshotRow :: (String, UTCTime, Int64, Text) -> ExceptT RepositoryError IO FinanceAccountSnapshot
    decodeSnapshotRow (snapshotId, snapshotOccurredAt, snapshotBalance, snapshotStatusText) =
      case parseFinanceAccountSnapshotReconciliationStatus snapshotStatusText of
        Nothing -> throwError ReadFailure
        Just snapshotStatus ->
          pure FinanceAccountSnapshot
            { financeAccountSnapshotId = snapshotId
            , financeAccountSnapshotOccurredAt = snapshotOccurredAt
            , financeAccountSnapshotBalance = fromIntegral snapshotBalance
            , financeAccountSnapshotReconciliationStatus = snapshotStatus
            }

pgListFinanceAccountSnapshotsView :: Pool Connection -> String -> ExceptT RepositoryError IO [FinanceAccountSnapshotView]
pgListFinanceAccountSnapshotsView pool userId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept
      (query conn
        "SELECT account_id, snapshot_id, occurred_at, balance, reconciliation_status FROM finance_balance_snapshots WHERE user_id = ? ORDER BY occurred_at DESC, snapshot_id ASC"
        (Only userId)
        :: IO [(String, String, UTCTime, Int64, Text)])
      mapSqlReadException
    mapM decodeSnapshotViewRow rows
  where
    decodeSnapshotViewRow :: (String, String, UTCTime, Int64, Text) -> ExceptT RepositoryError IO FinanceAccountSnapshotView
    decodeSnapshotViewRow (accountId, snapshotId, snapshotOccurredAt, snapshotBalance, snapshotStatusText) =
      case parseFinanceAccountSnapshotReconciliationStatus snapshotStatusText of
        Nothing -> throwError ReadFailure
        Just snapshotStatus ->
          pure FinanceAccountSnapshotView
            { financeAccountSnapshotViewAccountId = accountId
            , financeAccountSnapshotViewId = snapshotId
            , financeAccountSnapshotViewOccurredAt = snapshotOccurredAt
            , financeAccountSnapshotViewBalance = fromIntegral snapshotBalance
            , financeAccountSnapshotViewReconciliationStatus = snapshotStatus
            }

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
      "SELECT occurred_at, balance, reconciliation_status FROM finance_balance_snapshots WHERE user_id = ? AND account_id = ? AND snapshot_id = ?"
      (userId, accountId, snapshotId)
      :: IO [(UTCTime, Int64, Text)])
    mapSqlReadException
  (snapshotOccurredAt, observedBalance, snapshotStatusText) <- case snapshotRows of
    [(rowOccurredAt, rowBalance, rowStatusText)] -> pure (rowOccurredAt, rowBalance, rowStatusText)
    [] -> throwError NotFound
    _ -> throwError ReadFailure
  currentStatus <- case parseFinanceAccountSnapshotReconciliationStatus snapshotStatusText of
    Nothing -> throwError ReadFailure
    Just status -> pure status
  basisRow <-
    if currentStatus == FinanceAccountSnapshotReconciled
      then pure (Just (snapshotId, snapshotOccurredAt, observedBalance))
      else do
        rows <- tryExcept
          (query conn
            "SELECT snapshot_id, occurred_at, balance FROM finance_balance_snapshots WHERE user_id = ? AND account_id = ? AND reconciliation_status = 'reconciled' AND occurred_at <= ? ORDER BY occurred_at DESC, snapshot_id ASC LIMIT 1"
            (userId, accountId, snapshotOccurredAt)
            :: IO [(String, UTCTime, Int64)])
          mapSqlReadException
        case rows of
          [] -> pure Nothing
          [(basisSnapshotId, basisSnapshotOccurredAt, basisBalance)] ->
            pure (Just (basisSnapshotId, basisSnapshotOccurredAt, basisBalance))
          _ -> throwError ReadFailure
  (basisSnapshotId, basisSnapshotOccurredAt, basisBalance) <- case basisRow of
    Just (rowSnapshotId, rowOccurredAt, rowBalance) ->
      pure (Just rowSnapshotId, Just rowOccurredAt, rowBalance)
    Nothing ->
      pure (Nothing, Nothing, 0)
  sumRows <-
    case basisSnapshotOccurredAt of
      Nothing ->
        tryExcept
          (query conn
            "SELECT COALESCE(SUM(CASE direction WHEN 'received' THEN amount WHEN 'sent' THEN -amount ELSE 0 END), 0)::bigint FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at <= ?"
            (userId, accountId, snapshotOccurredAt)
            :: IO [Only Int64])
          mapSqlReadException
      Just basisOccurredAt ->
        tryExcept
          (query conn
            "SELECT COALESCE(SUM(CASE direction WHEN 'received' THEN amount WHEN 'sent' THEN -amount ELSE 0 END), 0)::bigint FROM finance_transactions WHERE user_id = ? AND account_id = ? AND occurred_at > ? AND occurred_at <= ?"
            (userId, accountId, basisOccurredAt, snapshotOccurredAt)
            :: IO [Only Int64])
          mapSqlReadException
  derivedDelta <- case sumRows of
    [Only total] -> pure total
    _ -> throwError ReadFailure
  let observedInt = fromIntegral observedBalance
      basisInt = fromIntegral basisBalance
      derivedInt = basisInt + fromIntegral derivedDelta
  pure FinanceAccountReconciliation
    { financeAccountReconciliationSnapshotId = snapshotId
    , financeAccountReconciliationSnapshotOccurredAt = snapshotOccurredAt
    , financeAccountReconciliationReconciliationStatus = currentStatus
    , financeAccountReconciliationObservedBalance = observedInt
    , financeAccountReconciliationDerivedBalanceAtSnapshot = derivedInt
    , financeAccountReconciliationDiscrepancy = observedInt - derivedInt
    , financeAccountReconciliationBasisSnapshotId = basisSnapshotId
    , financeAccountReconciliationBasisSnapshotOccurredAt = basisSnapshotOccurredAt
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

financeAccountSnapshotReconciliationStatusText :: FinanceAccountSnapshotReconciliationStatus -> Text
financeAccountSnapshotReconciliationStatusText FinanceAccountSnapshotUnreconciled = "unreconciled"
financeAccountSnapshotReconciliationStatusText FinanceAccountSnapshotReconciled = "reconciled"

parseFinanceAccountSnapshotReconciliationStatus :: Text -> Maybe FinanceAccountSnapshotReconciliationStatus
parseFinanceAccountSnapshotReconciliationStatus "unreconciled" = Just FinanceAccountSnapshotUnreconciled
parseFinanceAccountSnapshotReconciliationStatus "reconciled" = Just FinanceAccountSnapshotReconciled
parseFinanceAccountSnapshotReconciliationStatus _ = Nothing

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

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack
