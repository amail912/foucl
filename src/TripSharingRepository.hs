{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Pool (Pool, withResource)
import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , SqlError(..)
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

postgresTripSharingRepository :: Pool Connection -> TripSharingRepository
postgresTripSharingRepository pool =
  TripSharingRepository
    { repoListSharedUsers = pgListSharedUsers pool
    , repoAddSharedUser = pgAddSharedUser pool
    , repoDeleteSharedUser = pgDeleteSharedUser pool
    , repoListSubscribedUsers = pgListSubscribedUsers pool
    , repoAddSubscribedUser = pgAddSubscribedUser pool
    , repoDeleteSubscribedUser = pgDeleteSubscribedUser pool
    }

verifyPostgresTripSharingStorage :: Pool Connection -> IO (Either String ())
verifyPostgresTripSharingStorage pool = do
  verifyResult <- Ex.try $ withResource pool $ \conn -> do
      pingResult <- timedTry "SELECT ping-trip-sharing" (query_ conn "SELECT 1" :: IO [Only Int]) :: IO (Either Ex.SomeException [Only Int])
      sharesResult <- timedTry "SELECT trip-shares-schema-check" (query_ conn "SELECT owner_user_id, target_username FROM trip_shares LIMIT 0" :: IO [(String, String)]) :: IO (Either Ex.SomeException [(String, String)])
      subscriptionsResult <- timedTry "SELECT trip-subscriptions-schema-check" (query_ conn "SELECT owner_user_id, target_username FROM trip_subscriptions LIMIT 0" :: IO [(String, String)]) :: IO (Either Ex.SomeException [(String, String)])
      case pingResult of
        Left err -> pure (Left ("Postgres ping query failed: " ++ show err))
        Right _ ->
          case sharesResult of
            Left err -> pure (Left ("Trip-sharing schema check failed for trip_shares: " ++ show err))
            Right _ ->
              case subscriptionsResult of
                Left err -> pure (Left ("Trip-sharing schema check failed for trip_subscriptions: " ++ show err))
                Right _ -> pure (Right ())
  case verifyResult of
    Left err -> pure (Left ("Unable to connect to Postgres: " ++ show (err :: Ex.SomeException)))
    Right value -> pure value

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

pgListSharedUsers :: Pool Connection -> String -> ExceptT RepositoryError IO [String]
pgListSharedUsers pool ownerUserId =
  withPgConnection pool StorageFailure $ \conn -> do
    readResult <- liftIO (timedTry "SELECT trip-shares-by-owner"
      (query conn "SELECT target_username FROM trip_shares WHERE owner_user_id = ? ORDER BY target_username" (Only ownerUserId))
      :: IO (Either Ex.SomeException [Only String]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right usernames -> pure (map fromOnly usernames)

pgAddSharedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgAddSharedUser pool ownerUserId targetUsername =
  withPgConnection pool StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "INSERT trip-share"
      (execute conn
        "INSERT INTO trip_shares (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
        (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgDeleteSharedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteSharedUser pool ownerUserId targetUsername =
  withPgConnection pool StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "DELETE trip-share"
      (execute conn "DELETE FROM trip_shares WHERE owner_user_id = ? AND target_username = ?" (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgListSubscribedUsers :: Pool Connection -> String -> ExceptT RepositoryError IO [String]
pgListSubscribedUsers pool ownerUserId =
  withPgConnection pool StorageFailure $ \conn -> do
    readResult <- liftIO (timedTry "SELECT trip-subscriptions-by-owner"
      (query conn "SELECT target_username FROM trip_subscriptions WHERE owner_user_id = ? ORDER BY target_username" (Only ownerUserId))
      :: IO (Either Ex.SomeException [Only String]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right usernames -> pure (map fromOnly usernames)

pgAddSubscribedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgAddSubscribedUser pool ownerUserId targetUsername =
  withPgConnection pool StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "INSERT trip-subscription"
      (execute conn
        "INSERT INTO trip_subscriptions (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
        (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgDeleteSubscribedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteSubscribedUser pool ownerUserId targetUsername =
  withPgConnection pool StorageFailure $ \conn -> do
    writeResult <- liftIO (timedTry "DELETE trip-subscription"
      (execute conn "DELETE FROM trip_subscriptions WHERE owner_user_id = ? AND target_username = ?" (ownerUserId, targetUsername))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

withPgConnection :: forall a. Pool Connection -> RepositoryError -> (Connection -> ExceptT RepositoryError IO a) -> ExceptT RepositoryError IO a
withPgConnection pool connectionError action = do
  runResult <- liftIO (Ex.try (withResource pool (\conn -> runExceptT (action conn))) :: IO (Either Ex.SomeException (Either RepositoryError a)))
  case runResult of
    Left _ -> throwError connectionError
    Right (Left err) -> throwError err
    Right (Right value) -> pure value

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
