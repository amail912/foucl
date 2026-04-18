{-# LANGUAGE OverloadedStrings #-}

module TripSharingRepository
  ( TripSharingRepository(..)
  , defaultTripSharingRepository
  , filesystemTripSharingRepository
  , postgresTripSharingRepository
  , verifyPostgresTripSharingStorage
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.List (sort)
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
import Repository (RepositoryError(..))
import SqlTiming (timedTry)
import TripSharingStorage
  ( TripShareStorageError(..)
  , TripShareStorageConfig
  , TripSubscriptionStorageConfig
  , addSharedUser
  , addSubscribedUser
  , defaultTripShareStorageConfig
  , defaultTripSubscriptionStorageConfig
  , deleteSharedUser
  , deleteSubscribedUser
  , getSharedUsers
  , getSubscribedUsers
  )
import qualified Control.Exception as Ex

data TripSharingRepository = TripSharingRepository
  { repoListSharedUsers :: String -> ExceptT RepositoryError IO [String]
  , repoAddSharedUser :: String -> String -> ExceptT RepositoryError IO ()
  , repoDeleteSharedUser :: String -> String -> ExceptT RepositoryError IO ()
  , repoListSubscribedUsers :: String -> ExceptT RepositoryError IO [String]
  , repoAddSubscribedUser :: String -> String -> ExceptT RepositoryError IO ()
  , repoDeleteSubscribedUser :: String -> String -> ExceptT RepositoryError IO ()
  }

defaultTripSharingRepository :: TripSharingRepository
defaultTripSharingRepository = filesystemTripSharingRepository defaultTripShareStorageConfig defaultTripSubscriptionStorageConfig

filesystemTripSharingRepository :: TripShareStorageConfig -> TripSubscriptionStorageConfig -> TripSharingRepository
filesystemTripSharingRepository shareConfig subscriptionConfig =
  TripSharingRepository
    { repoListSharedUsers = fsListSharedUsers shareConfig
    , repoAddSharedUser = fsAddSharedUser shareConfig
    , repoDeleteSharedUser = fsDeleteSharedUser shareConfig
    , repoListSubscribedUsers = fsListSubscribedUsers subscriptionConfig
    , repoAddSubscribedUser = fsAddSubscribedUser subscriptionConfig
    , repoDeleteSubscribedUser = fsDeleteSubscribedUser subscriptionConfig
    }

postgresTripSharingRepository :: String -> TripSharingRepository
postgresTripSharingRepository connectionString =
  TripSharingRepository
    { repoListSharedUsers = pgListSharedUsers connectionString
    , repoAddSharedUser = pgAddSharedUser connectionString
    , repoDeleteSharedUser = pgDeleteSharedUser connectionString
    , repoListSubscribedUsers = pgListSubscribedUsers connectionString
    , repoAddSubscribedUser = pgAddSubscribedUser connectionString
    , repoDeleteSubscribedUser = pgDeleteSubscribedUser connectionString
    }

verifyPostgresTripSharingStorage :: String -> IO (Either String ())
verifyPostgresTripSharingStorage connectionString = do
  connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
  case connResult of
    Left err -> pure (Left ("Unable to connect to Postgres: " ++ show err))
    Right conn -> do
      pingResult <- timedTry "SELECT ping-trip-sharing" (query_ conn "SELECT 1" :: IO [Only Int]) :: IO (Either Ex.SomeException [Only Int])
      sharesResult <- timedTry "SELECT trip-shares-schema-check" (query_ conn "SELECT owner_user_id, target_username FROM trip_shares LIMIT 0" :: IO [(String, String)]) :: IO (Either Ex.SomeException [(String, String)])
      subscriptionsResult <- timedTry "SELECT trip-subscriptions-schema-check" (query_ conn "SELECT owner_user_id, target_username FROM trip_subscriptions LIMIT 0" :: IO [(String, String)]) :: IO (Either Ex.SomeException [(String, String)])
      _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
      case pingResult of
        Left err -> pure (Left ("Postgres ping query failed: " ++ show err))
        Right _ ->
          case sharesResult of
            Left err -> pure (Left ("Trip-sharing schema check failed for trip_shares: " ++ show err))
            Right _ ->
              case subscriptionsResult of
                Left err -> pure (Left ("Trip-sharing schema check failed for trip_subscriptions: " ++ show err))
                Right _ -> pure (Right ())

fsListSharedUsers :: TripShareStorageConfig -> String -> ExceptT RepositoryError IO [String]
fsListSharedUsers shareConfig ownerUserId = do
  result <- liftIO (getSharedUsers shareConfig ownerUserId)
  case result of
    Left err -> throwError (mapTripSharingError err)
    Right usernames -> pure (sort usernames)

fsAddSharedUser :: TripShareStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsAddSharedUser shareConfig ownerUserId targetUsername = do
  result <- liftIO (addSharedUser shareConfig ownerUserId targetUsername)
  case result of
    Left err -> throwError (mapTripSharingError err)
    Right () -> pure ()

fsDeleteSharedUser :: TripShareStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsDeleteSharedUser shareConfig ownerUserId targetUsername = do
  result <- liftIO (deleteSharedUser shareConfig ownerUserId targetUsername)
  case result of
    Left err -> throwError (mapTripSharingError err)
    Right () -> pure ()

fsListSubscribedUsers :: TripSubscriptionStorageConfig -> String -> ExceptT RepositoryError IO [String]
fsListSubscribedUsers subscriptionConfig ownerUserId = do
  result <- liftIO (getSubscribedUsers subscriptionConfig ownerUserId)
  case result of
    Left err -> throwError (mapTripSharingError err)
    Right usernames -> pure (sort usernames)

fsAddSubscribedUser :: TripSubscriptionStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsAddSubscribedUser subscriptionConfig ownerUserId targetUsername = do
  result <- liftIO (addSubscribedUser subscriptionConfig ownerUserId targetUsername)
  case result of
    Left err -> throwError (mapTripSharingError err)
    Right () -> pure ()

fsDeleteSubscribedUser :: TripSubscriptionStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsDeleteSubscribedUser subscriptionConfig ownerUserId targetUsername = do
  result <- liftIO (deleteSubscribedUser subscriptionConfig ownerUserId targetUsername)
  case result of
    Left err -> throwError (mapTripSharingError err)
    Right () -> pure ()

mapTripSharingError :: TripShareStorageError -> RepositoryError
mapTripSharingError TripShareReadFailure = ReadFailure
mapTripSharingError TripShareWriteFailure = WriteFailure

pgListSharedUsers :: String -> String -> ExceptT RepositoryError IO [String]
pgListSharedUsers connectionString ownerUserId =
  withPgConnection connectionString StorageFailure $ \conn -> do
    readResult <- liftIO (timedTry "SELECT trip-shares-by-owner"
      (query conn "SELECT target_username FROM trip_shares WHERE owner_user_id = ? ORDER BY target_username" (Only ownerUserId))
      :: IO (Either Ex.SomeException [Only String]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right usernames -> pure (map fromOnly usernames)

pgAddSharedUser :: String -> String -> String -> ExceptT RepositoryError IO ()
pgAddSharedUser connectionString ownerUserId targetUsername =
  withPgConnection connectionString StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "INSERT trip-share"
      (execute conn
        "INSERT INTO trip_shares (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
        (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgDeleteSharedUser :: String -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteSharedUser connectionString ownerUserId targetUsername =
  withPgConnection connectionString StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "DELETE trip-share"
      (execute conn "DELETE FROM trip_shares WHERE owner_user_id = ? AND target_username = ?" (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgListSubscribedUsers :: String -> String -> ExceptT RepositoryError IO [String]
pgListSubscribedUsers connectionString ownerUserId =
  withPgConnection connectionString StorageFailure $ \conn -> do
    readResult <- liftIO (timedTry "SELECT trip-subscriptions-by-owner"
      (query conn "SELECT target_username FROM trip_subscriptions WHERE owner_user_id = ? ORDER BY target_username" (Only ownerUserId))
      :: IO (Either Ex.SomeException [Only String]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right usernames -> pure (map fromOnly usernames)

pgAddSubscribedUser :: String -> String -> String -> ExceptT RepositoryError IO ()
pgAddSubscribedUser connectionString ownerUserId targetUsername =
  withPgConnection connectionString StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "INSERT trip-subscription"
      (execute conn
        "INSERT INTO trip_subscriptions (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
        (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgDeleteSubscribedUser :: String -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteSubscribedUser connectionString ownerUserId targetUsername =
  withPgConnection connectionString StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "DELETE trip-subscription"
      (execute conn "DELETE FROM trip_subscriptions WHERE owner_user_id = ? AND target_username = ?" (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

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
      | isStorageSqlError sqlErr -> StorageFailure
      | otherwise -> WriteFailure
    Nothing -> StorageFailure

isStorageSqlError :: SqlError -> Bool
isStorageSqlError sqlErr = "08" `BS8.isPrefixOf` sqlState sqlErr
