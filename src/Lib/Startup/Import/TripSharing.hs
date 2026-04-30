{-# LANGUAGE OverloadedStrings #-}

module Lib.Startup.Import.TripSharing
  ( runTripSharingStartupImport
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (decode)
import Data.List (nub, sort, sortOn)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection, execute, query_)
import Helpers (tryExcept, withResourceMHandled)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)

runTripSharingStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runTripSharingStartupImport pool cd = do
  let sharesBaseDir = cd </> "data" </> "trip-sharing" </> "shares"
      subscriptionsBaseDir = cd </> "data" </> "trip-sharing" </> "subscriptions"
  fsShares <- loadFilesystemOwnerUserPairs sharesBaseDir "shares"
  fsSubscriptions <- loadFilesystemOwnerUserPairs subscriptionsBaseDir "subscriptions"
  (sharesImported, sharesSkipped, subsImported, subsSkipped, totalPgShares, totalPgSubs) <-
    withResourceMHandled
      (\err -> "Trip-sharing startup import failed while connecting to Postgres: " ++ show err)
      pool
      (\conn -> do
        pgShares <- tryExcept (query_ conn "SELECT owner_user_id, target_username FROM trip_shares" :: IO [(String, String)])
                              (\err -> "Trip-sharing startup import failed while reading Postgres shares: " ++ show err)
        pgSubscriptions <- tryExcept (query_ conn "SELECT owner_user_id, target_username FROM trip_subscriptions" :: IO [(String, String)])
                                     (\err -> "Trip-sharing startup import failed while reading Postgres subscriptions: " ++ show err)
        let orderedFsShares = sortOn id fsShares
            orderedFsSubscriptions = sortOn id fsSubscriptions
            pgShareKeys = Set.fromList pgShares
            pgSubscriptionKeys = Set.fromList pgSubscriptions
            filesystemCount = length orderedFsShares + length orderedFsSubscriptions
            postgresCount = length pgShares + length pgSubscriptions
        when (filesystemCount > 0 && postgresCount > 0) $
          lift $ putStrLn
            ( "[startup][trip-sharing-import][warning] overlap detected:"
                ++ " filesystem_shares="
                ++ show (length orderedFsShares)
                ++ " filesystem_subscriptions="
                ++ show (length orderedFsSubscriptions)
                ++ " postgres_shares="
                ++ show (length pgShares)
                ++ " postgres_subscriptions="
                ++ show (length pgSubscriptions)
                ++ " conflict_policy=postgres-wins"
            )
        (si, ss, _) <- foldM (importSingleTripShare conn) (0 :: Int, 0 :: Int, pgShareKeys) orderedFsShares
        (sui, sus, _) <- foldM (importSingleTripSubscription conn) (0 :: Int, 0 :: Int, pgSubscriptionKeys) orderedFsSubscriptions
        pure (si, ss, sui, sus, length pgShares, length pgSubscriptions)
      )
  lift $ putStrLn
    ( "[startup][trip-sharing-import] completed"
        ++ " filesystem_shares="
        ++ show (length fsShares)
        ++ " filesystem_subscriptions="
        ++ show (length fsSubscriptions)
        ++ " postgres_shares="
        ++ show totalPgShares
        ++ " postgres_subscriptions="
        ++ show totalPgSubs
        ++ " imported_shares="
        ++ show sharesImported
        ++ " imported_subscriptions="
        ++ show subsImported
        ++ " skipped_share_conflicts="
        ++ show sharesSkipped
        ++ " skipped_subscription_conflicts="
        ++ show subsSkipped
    )

importSingleTripShare
  :: Connection
  -> (Int, Int, Set.Set (String, String))
  -> (String, String)
  -> ExceptT String IO (Int, Int, Set.Set (String, String))
importSingleTripShare conn (importedCount, skippedCount, knownKeys) (ownerUserId, targetUsername) =
  if Set.member (ownerUserId, targetUsername) knownKeys
    then do
      lift $ putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting share owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownKeys)
    else do
      affected <- tryExcept
        (execute conn
          "INSERT INTO trip_shares (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
          (ownerUserId, targetUsername))
        (\err -> "Trip-sharing startup import failed while writing share owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ show err)
      if affected > 0
        then
          pure (importedCount + 1, skippedCount, Set.insert (ownerUserId, targetUsername) knownKeys)
        else do
          lift $ putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting share owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert (ownerUserId, targetUsername) knownKeys)

importSingleTripSubscription
  :: Connection
  -> (Int, Int, Set.Set (String, String))
  -> (String, String)
  -> ExceptT String IO (Int, Int, Set.Set (String, String))
importSingleTripSubscription conn (importedCount, skippedCount, knownKeys) (ownerUserId, targetUsername) =
  if Set.member (ownerUserId, targetUsername) knownKeys
    then do
      lift $ putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting subscription owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownKeys)
    else do
      affected <- tryExcept
        (execute conn
          "INSERT INTO trip_subscriptions (owner_user_id, target_username) VALUES (?, ?) ON CONFLICT (owner_user_id, target_username) DO NOTHING"
          (ownerUserId, targetUsername))
        (\err -> "Trip-sharing startup import failed while writing subscription owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ show err)
      if affected > 0
        then
          pure (importedCount + 1, skippedCount, Set.insert (ownerUserId, targetUsername) knownKeys)
        else do
          lift $ putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting subscription owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert (ownerUserId, targetUsername) knownKeys)

loadFilesystemOwnerUserPairs :: FilePath -> String -> ExceptT String IO [(String, String)]
loadFilesystemOwnerUserPairs rootDir relationLabel = do
  exists <- lift $ doesDirectoryExist rootDir
  if not exists
    then do
      lift $ putStrLn ("[startup][trip-sharing-import] source " ++ relationLabel ++ " directory is missing; treating filesystem trip-sharing " ++ relationLabel ++ " source as empty")
      pure []
    else do
      entries <- lift $ listDirectory rootDir
      foldM (loadSingleOwnerUserPairs rootDir relationLabel) [] (sort entries)

loadSingleOwnerUserPairs
  :: FilePath
  -> String
  -> [(String, String)]
  -> FilePath
  -> ExceptT String IO [(String, String)]
loadSingleOwnerUserPairs rootDir relationLabel acc fileName
  | takeExtension fileName /= ".json" = pure acc
  | otherwise = do
      let fullPath = rootDir </> fileName
          ownerUserId = takeBaseName fileName
      content <- lift $ BL.readFile fullPath
      case decode content of
        Nothing ->
          throwE ("Trip-sharing startup import failed while reading filesystem " ++ relationLabel ++ " for owner_user_id=" ++ ownerUserId ++ ": invalid JSON in " ++ fullPath)
        Just usernames ->
          let normalized = sort (nub (usernames :: [String]))
              pairs = [(ownerUserId, username) | username <- normalized]
           in pure (pairs ++ acc)
