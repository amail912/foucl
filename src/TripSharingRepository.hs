module TripSharingRepository
  ( TripSharingRepository(..)
  , defaultTripSharingRepository
  , filesystemTripSharingRepository
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Repository (RepositoryError(..))
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
