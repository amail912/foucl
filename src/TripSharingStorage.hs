{-# LANGUAGE NamedFieldPuns #-}

module TripSharingStorage
  ( TripShareStorageConfig(..)
  , defaultTripShareStorageConfig
  , TripSubscriptionStorageConfig(..)
  , defaultTripSubscriptionStorageConfig
  , TripShareStorageError(..)
  , getSharedUsers
  , addSharedUser
  , deleteSharedUser
  , getSubscribedUsers
  , addSubscribedUser
  , deleteSubscribedUser
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

newtype TripShareStorageConfig = TripShareStorageConfig
  { tripShareRootPath :: FilePath
  }

defaultTripShareStorageConfig :: TripShareStorageConfig
defaultTripShareStorageConfig = TripShareStorageConfig "data/trip-sharing/shares"

newtype TripSubscriptionStorageConfig = TripSubscriptionStorageConfig
  { tripSubscriptionRootPath :: FilePath
  }

defaultTripSubscriptionStorageConfig :: TripSubscriptionStorageConfig
defaultTripSubscriptionStorageConfig = TripSubscriptionStorageConfig "data/trip-sharing/subscriptions"

data TripShareStorageError
  = TripShareReadFailure
  | TripShareWriteFailure
  deriving (Show, Eq)

getSharedUsers :: TripShareStorageConfig -> String -> IO (Either TripShareStorageError [String])
getSharedUsers config ownerUserId = do
  getUsersFromFile (tripShareRootPath config) (ownerUserFilePath (tripShareRootPath config) ownerUserId)

addSharedUser :: TripShareStorageConfig -> String -> String -> IO (Either TripShareStorageError ())
addSharedUser config ownerUserId targetUsername = do
  existingResult <- getSharedUsers config ownerUserId
  case existingResult of
    Left err -> pure (Left err)
    Right usernames -> writeUsersToFile (tripShareRootPath config) (ownerUserFilePath (tripShareRootPath config) ownerUserId) (targetUsername : usernames)

deleteSharedUser :: TripShareStorageConfig -> String -> String -> IO (Either TripShareStorageError ())
deleteSharedUser config ownerUserId targetUsername = do
  existingResult <- getSharedUsers config ownerUserId
  case existingResult of
    Left err -> pure (Left err)
    Right usernames ->
      if targetUsername `notElem` usernames
        then pure (Right ())
        else writeUsersToFile (tripShareRootPath config) (ownerUserFilePath (tripShareRootPath config) ownerUserId) (filter (/= targetUsername) usernames)

getSubscribedUsers :: TripSubscriptionStorageConfig -> String -> IO (Either TripShareStorageError [String])
getSubscribedUsers config ownerUserId =
  getUsersFromFile (tripSubscriptionRootPath config) (ownerUserFilePath (tripSubscriptionRootPath config) ownerUserId)

addSubscribedUser :: TripSubscriptionStorageConfig -> String -> String -> IO (Either TripShareStorageError ())
addSubscribedUser config ownerUserId targetUsername = do
  existingResult <- getSubscribedUsers config ownerUserId
  case existingResult of
    Left err -> pure (Left err)
    Right usernames -> writeUsersToFile (tripSubscriptionRootPath config) (ownerUserFilePath (tripSubscriptionRootPath config) ownerUserId) (targetUsername : usernames)

deleteSubscribedUser :: TripSubscriptionStorageConfig -> String -> String -> IO (Either TripShareStorageError ())
deleteSubscribedUser config ownerUserId targetUsername = do
  existingResult <- getSubscribedUsers config ownerUserId
  case existingResult of
    Left err -> pure (Left err)
    Right usernames ->
      if targetUsername `notElem` usernames
        then pure (Right ())
        else writeUsersToFile (tripSubscriptionRootPath config) (ownerUserFilePath (tripSubscriptionRootPath config) ownerUserId) (filter (/= targetUsername) usernames)

getUsersFromFile :: FilePath -> FilePath -> IO (Either TripShareStorageError [String])
getUsersFromFile rootPath path = do
  createDirectoryIfMissing True rootPath
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      contentOrErr <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
      case contentOrErr of
        Left _ -> pure (Left TripShareReadFailure)
        Right raw ->
          case decode raw of
            Nothing -> pure (Left TripShareReadFailure)
            Just usernames -> pure (Right (normalizeUsernames usernames))

writeUsersToFile :: FilePath -> FilePath -> [String] -> IO (Either TripShareStorageError ())
writeUsersToFile rootPath path usernames = do
  createDirectoryIfMissing True rootPath
  writeResult <- try (BL.writeFile path (encode normalized)) :: IO (Either IOException ())
  case writeResult of
    Left _ -> pure (Left TripShareWriteFailure)
    Right _ -> pure (Right ())
  where
    normalized = normalizeUsernames usernames

normalizeUsernames :: [String] -> [String]
normalizeUsernames = sort . nub

ownerUserFilePath :: FilePath -> String -> FilePath
ownerUserFilePath rootPath ownerUserId = rootPath </> ownerUserId ++ ".json"
