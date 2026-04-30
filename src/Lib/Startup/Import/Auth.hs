{-# LANGUAGE LambdaCase #-}

module Lib.Startup.Import.Auth
  ( runAuthStartupImport
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, catchE)
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Auth (AuthRepository, defaultAuthRepository)
import qualified AuthRepository
import Repository (RepositoryError(..))

runAuthStartupImport :: Pool Connection -> ExceptT String IO ()
runAuthStartupImport pool = do
  let postgresRepo = AuthRepository.postgresAuthRepository pool
  fsUsers <- catchE (AuthRepository.repoListUsers defaultAuthRepository)
                    (\case
                        StorageFailure -> do
                          lift $ putStrLn "[startup][auth-import] source users directory is missing; treating filesystem auth source as empty"
                          pure []
                        e -> throwE ("Auth startup import failed while reading filesystem users: " ++ show e))
  pgUsers <- catchE (AuthRepository.repoListUsers postgresRepo)
                    (\err -> throwE ("Auth startup import failed while reading Postgres users: " ++ show err))
  let orderedFsUsers = sortOn AuthRepository.uname fsUsers
      pgUsernames = Set.fromList (map AuthRepository.uname pgUsers)
  when (not (null orderedFsUsers) && not (null pgUsers)) $
    lift $ putStrLn ("[startup][auth-import][warning] overlap detected: filesystem_count=" ++ show (length orderedFsUsers) ++ " postgres_count=" ++ show (length pgUsers) ++ " conflict_policy=postgres-wins")
  (importedCount, skippedCount) <- foldM (importSingleAuthUser postgresRepo pgUsernames) (0 :: Int, 0 :: Int) orderedFsUsers
  lift $ putStrLn ("[startup][auth-import] completed filesystem_count=" ++ show (length orderedFsUsers) ++ " postgres_count=" ++ show (length pgUsers) ++ " imported=" ++ show importedCount ++ " skipped_conflicts=" ++ show skippedCount)

importSingleAuthUser :: AuthRepository -> Set.Set String -> (Int, Int) -> AuthRepository.PersistedUser -> ExceptT String IO (Int, Int)
importSingleAuthUser postgresRepo pgUsernames (importedCount, skippedCount) fsUser =
  if Set.member username pgUsernames
    then do
      lift $ putStrLn ("[startup][auth-import][warning] skipping conflicting username=" ++ username ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1)
    else do
      catchE
        (AuthRepository.repoCreateUser postgresRepo fsUser >> pure (importedCount + 1, skippedCount))
        (\err ->
          case err of
            AlreadyExists -> do
              lift $ putStrLn ("[startup][auth-import][warning] skipping conflicting username=" ++ username ++ " policy=postgres-wins")
              pure (importedCount, skippedCount + 1)
            _ -> throwE ("Auth startup import failed while writing username=" ++ username ++ ": " ++ show err))
  where
    username = AuthRepository.uname fsUser
