{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FinanceAccountRepository
  ( FinanceAccountRepository(..)
  , FinanceAccount(..)
  , FinanceAccountCreateRequest(..)
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
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
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
  }

data FinanceAccountCreateRequest = FinanceAccountCreateRequest
  { financeAccountCreateName :: !String
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

instance FromJSON FinanceAccountCreateRequest where
  parseJSON = withObject "FinanceAccountCreateRequest" $ \value ->
    FinanceAccountCreateRequest <$> value .: "name"

instance ToJSON FinanceAccount where
  toJSON FinanceAccount { financeAccountId, financeAccountName, financeAccountStatus } =
    object
      [ "id" .= financeAccountId
      , "name" .= financeAccountName
      , "status" .= financeAccountStatusText financeAccountStatus
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
