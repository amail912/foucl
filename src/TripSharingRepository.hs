{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TripSharingRepository
  ( TripSharingRepository(..)
  , defaultTripSharingRepository
  , filesystemTripSharingRepository
  , postgresTripSharingRepository
  , tripSharingPostgresHealthChecks
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.List (sort)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , execute
  , query
  , query_
  )
import Repository (RepositoryError(..))
import Helpers (tryExcept, withPoolExceptHandled, withResourceMHandled, mapSqlReadException, mapSqlWriteExceptionNoConflict)
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

data TripSharingRepository = TripSharingRepository
  { repoListSharedUsers :: !(String -> ExceptT RepositoryError IO [String])
  , repoAddSharedUser :: !(String -> String -> ExceptT RepositoryError IO ())
  , repoDeleteSharedUser :: !(String -> String -> ExceptT RepositoryError IO ())
  , repoListSubscribedUsers :: !(String -> ExceptT RepositoryError IO [String])
  , repoAddSubscribedUser :: !(String -> String -> ExceptT RepositoryError IO ())
  , repoDeleteSubscribedUser :: !(String -> String -> ExceptT RepositoryError IO ())
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

tripSharingPostgresHealthChecks :: Connection -> ExceptT String IO ()
tripSharingPostgresHealthChecks conn = do
  tryExcept (query_ conn "SELECT owner_user_id, target_username FROM trip_shares LIMIT 0" :: IO [(String, String)])
            (\err -> "Trip-sharing schema check failed for trip_shares: " ++ show err)
  tryExcept (query_ conn "SELECT owner_user_id, target_username FROM trip_subscriptions LIMIT 0" :: IO [(String, String)])
            (\err -> "Trip-sharing schema check failed for trip_subscriptions: " ++ show err)
  pure ()

fsListSharedUsers :: TripShareStorageConfig -> String -> ExceptT RepositoryError IO [String]
fsListSharedUsers shareConfig ownerUserId = do
  result <- liftIO (getSharedUsers shareConfig ownerUserId)
  either (throwError . mapTripSharingError) pure result

fsAddSharedUser :: TripShareStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsAddSharedUser shareConfig ownerUserId targetUsername = do
  result <- liftIO (addSharedUser shareConfig ownerUserId targetUsername)
  either (throwError . mapTripSharingError) pure result

fsDeleteSharedUser :: TripShareStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsDeleteSharedUser shareConfig ownerUserId targetUsername = do
  result <- liftIO (deleteSharedUser shareConfig ownerUserId targetUsername)
  either (throwError . mapTripSharingError) pure result

fsListSubscribedUsers :: TripSubscriptionStorageConfig -> String -> ExceptT RepositoryError IO [String]
fsListSubscribedUsers subscriptionConfig ownerUserId = do
  result <- liftIO (getSubscribedUsers subscriptionConfig ownerUserId)
  either (throwError . mapTripSharingError) (pure . sort) result

fsAddSubscribedUser :: TripSubscriptionStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsAddSubscribedUser subscriptionConfig ownerUserId targetUsername = do
  result <- liftIO (addSubscribedUser subscriptionConfig ownerUserId targetUsername)
  either (throwError . mapTripSharingError) pure result

fsDeleteSubscribedUser :: TripSubscriptionStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsDeleteSubscribedUser subscriptionConfig ownerUserId targetUsername = do
  result <- liftIO (deleteSubscribedUser subscriptionConfig ownerUserId targetUsername)
  either (throwError . mapTripSharingError) pure result

mapTripSharingError :: TripShareStorageError -> RepositoryError
mapTripSharingError TripShareReadFailure = ReadFailure
mapTripSharingError TripShareWriteFailure = WriteFailure

pgListSharedUsers :: Pool Connection -> String -> ExceptT RepositoryError IO [String]
pgListSharedUsers pool ownerUserId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    usernames <-tryExcept
      (query conn "SELECT target_username FROM trip_shares WHERE owner_user_id = ? ORDER BY target_username" (Only ownerUserId))
      mapSqlReadException
    pure (map fromOnly usernames)

pgAddSharedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgAddSharedUser pool ownerUserId targetUsername =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    writeResult <- tryExcept
      (execute conn
        "INSERT INTO trip_shares (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
        (ownerUserId, targetUsername))
        mapSqlWriteExceptionNoConflict
    pure ()

pgDeleteSharedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteSharedUser pool ownerUserId targetUsername =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    writeResult <- tryExcept
      (execute conn "DELETE FROM trip_shares WHERE owner_user_id = ? AND target_username = ?" (ownerUserId, targetUsername))
      mapSqlWriteExceptionNoConflict
    pure ()

pgListSubscribedUsers :: Pool Connection -> String -> ExceptT RepositoryError IO [String]
pgListSubscribedUsers pool ownerUserId =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    usernames <-tryExcept
      (query conn "SELECT target_username FROM trip_subscriptions WHERE owner_user_id = ? ORDER BY target_username" (Only ownerUserId))
      mapSqlReadException
    pure (map fromOnly usernames)

pgAddSubscribedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgAddSubscribedUser pool ownerUserId targetUsername =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    writeResult <- tryExcept
      (execute conn
        "INSERT INTO trip_subscriptions (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
        (ownerUserId, targetUsername))
        mapSqlWriteExceptionNoConflict
    pure ()

pgDeleteSubscribedUser :: Pool Connection -> String -> String -> ExceptT RepositoryError IO ()
pgDeleteSubscribedUser pool ownerUserId targetUsername =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    writeResult <- tryExcept
      (execute conn "DELETE FROM trip_subscriptions WHERE owner_user_id = ? AND target_username = ?" (ownerUserId, targetUsername))
      mapSqlWriteExceptionNoConflict
    pure ()
