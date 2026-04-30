{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Startup.Import.Session
  ( runSessionStartupImport
  , decodeJsonDirectory
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import Data.Aeson (FromJSON, decode)
import Data.Bifunctor (second)
import Data.List (sortOn)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL
import Data.Pool (Pool)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple (Connection, query_)
import Helpers (tryExcept, withResourceMHandled)
import Lib.Startup.Import.Common (withErrPrefix)
import Repository (RepositoryError(..))
import Session (SessionHandle(..), SessionRepository(..), SessionState(..), UserStateBinding(..), mkPostgresSessionRepository)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)

runSessionStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runSessionStartupImport pool cd = do
  let sessionBaseDir = cd </> "data" </> "sessions"
  let postgresRepo = mkPostgresSessionRepository pool
  (fsStates, fsHandles, fsBindings) <- loadFilesystemSessionImportSource sessionBaseDir
  (pgStates, pgHandles, pgBindings) <- loadPostgresSessionImportSnapshot pool
  let orderedFsStates = sortOn stateId fsStates
      orderedFsHandles = sortOn handleSessionId fsHandles
      orderedFsBindings = sortOn fst fsBindings
      pgStateIds = Set.fromList (map stateId pgStates)
      pgSessionIds = Set.fromList (map handleSessionId pgHandles)
      pgBindingUserIds = Set.fromList (map fst pgBindings)
      filesystemCount = length orderedFsStates + length orderedFsHandles + length orderedFsBindings
      postgresCount = length pgStates + length pgHandles + length pgBindings
  when (filesystemCount > 0 && postgresCount > 0) $
    lift $ putStrLn
      ( "[startup][session-import][warning] overlap detected:"
          ++ " filesystem_states="
          ++ show (length orderedFsStates)
          ++ " filesystem_handles="
          ++ show (length orderedFsHandles)
          ++ " filesystem_bindings="
          ++ show (length orderedFsBindings)
          ++ " postgres_states="
          ++ show (length pgStates)
          ++ " postgres_handles="
          ++ show (length pgHandles)
          ++ " postgres_bindings="
          ++ show (length pgBindings)
          ++ " conflict_policy=postgres-wins"
      )
  (statesImported, statesSkipped, _) <- foldM (importSingleSessionState postgresRepo) (0 :: Int, 0 :: Int, pgStateIds) orderedFsStates
  (handlesImported, handlesSkipped, _) <- foldM (importSingleSessionHandle postgresRepo) (0 :: Int, 0 :: Int, pgSessionIds) orderedFsHandles
  (bindingsImported, bindingsSkipped, _) <- foldM (importSingleSessionUserBinding postgresRepo) (0 :: Int, 0 :: Int, pgBindingUserIds) orderedFsBindings
  lift $ putStrLn
    ( "[startup][session-import] completed"
        ++ " filesystem_states="
        ++ show (length orderedFsStates)
        ++ " filesystem_handles="
        ++ show (length orderedFsHandles)
        ++ " filesystem_bindings="
        ++ show (length orderedFsBindings)
        ++ " postgres_states="
        ++ show (length pgStates)
        ++ " postgres_handles="
        ++ show (length pgHandles)
        ++ " postgres_bindings="
        ++ show (length pgBindings)
        ++ " imported_states="
        ++ show statesImported
        ++ " imported_handles="
        ++ show handlesImported
        ++ " imported_bindings="
        ++ show bindingsImported
        ++ " skipped_state_conflicts="
        ++ show statesSkipped
        ++ " skipped_handle_conflicts="
        ++ show handlesSkipped
        ++ " skipped_binding_conflicts="
        ++ show bindingsSkipped
    )

importSingleSessionState
  :: SessionRepository
  -> (Int, Int, Set.Set String)
  -> SessionState
  -> ExceptT String IO (Int, Int, Set.Set String)
importSingleSessionState postgresRepo (importedCount, skippedCount, knownStateIds) sessionState =
  if Set.member stateKey knownStateIds
    then do
      lift $ putStrLn ("[startup][session-import][warning] skipping conflicting state_id=" ++ stateKey ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownStateIds)
    else do
      catchE
        (repoCreateSessionState postgresRepo sessionState >> pure (importedCount + 1, skippedCount, Set.insert stateKey knownStateIds))
        (\err ->
          case err of
            AlreadyExists -> do
              lift $ putStrLn ("[startup][session-import][warning] skipping conflicting state_id=" ++ stateKey ++ " policy=postgres-wins")
              pure (importedCount, skippedCount + 1, Set.insert stateKey knownStateIds)
            _ -> throwE ("Session startup import failed while writing state_id=" ++ stateKey ++ ": " ++ show err))
  where
    stateKey = stateId sessionState

importSingleSessionHandle
  :: SessionRepository
  -> (Int, Int, Set.Set String)
  -> SessionHandle
  -> ExceptT String IO (Int, Int, Set.Set String)
importSingleSessionHandle postgresRepo (importedCount, skippedCount, knownSessionIds) sessionHandle =
  if Set.member sessionKey knownSessionIds
    then do
      lift $ putStrLn ("[startup][session-import][warning] skipping conflicting session_id=" ++ sessionKey ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownSessionIds)
    else do
      catchE
        (repoCreateSessionHandle postgresRepo sessionHandle >> pure (importedCount + 1, skippedCount, Set.insert sessionKey knownSessionIds))
        (\err ->
          case err of
            AlreadyExists -> do
              lift $ putStrLn ("[startup][session-import][warning] skipping conflicting session_id=" ++ sessionKey ++ " policy=postgres-wins")
              pure (importedCount, skippedCount + 1, Set.insert sessionKey knownSessionIds)
            _ -> throwE ("Session startup import failed while writing session_id=" ++ sessionKey ++ ": " ++ show err))
  where
    sessionKey = handleSessionId sessionHandle

importSingleSessionUserBinding
  :: SessionRepository
  -> (Int, Int, Set.Set String)
  -> (String, UserStateBinding)
  -> ExceptT String IO (Int, Int, Set.Set String)
importSingleSessionUserBinding postgresRepo (importedCount, skippedCount, knownUserIds) (userId, binding) =
  if Set.member userId knownUserIds
    then do
      lift $ putStrLn ("[startup][session-import][warning] skipping conflicting user_id=" ++ userId ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownUserIds)
    else do
      catchE
        (repoCreateUserStateBinding postgresRepo userId binding >> pure (importedCount + 1, skippedCount, Set.insert userId knownUserIds))
        (\err ->
          case err of
            AlreadyExists -> do
              lift $ putStrLn ("[startup][session-import][warning] skipping conflicting user_id=" ++ userId ++ " policy=postgres-wins")
              pure (importedCount, skippedCount + 1, Set.insert userId knownUserIds)
            _ -> throwE ("Session startup import failed while writing user_id=" ++ userId ++ ": " ++ show err))

loadFilesystemSessionImportSource :: FilePath -> ExceptT String IO ([SessionState], [SessionHandle], [(String, UserStateBinding)])
loadFilesystemSessionImportSource baseDir = do
  baseExists <- lift $ doesDirectoryExist baseDir
  if not baseExists
    then do
      lift $ putStrLn "[startup][session-import] source sessions directory is missing; treating filesystem session source as empty"
      pure ([], [], [])
    else do
      states <- withErrPrefix "Session startup import failed while reading filesystem states: " (decodeJsonDirectory (baseDir </> "states"))
      handles <- withErrPrefix "Session startup import failed while reading filesystem handles: " (decodeJsonDirectory (baseDir </> "handles"))
      bindings <- withErrPrefix "Session startup import failed while reading filesystem user bindings: " (decodeSessionBindingsDirectory (baseDir </> "users"))
      pure (states, handles, bindings)

loadPostgresSessionImportSnapshot :: Pool Connection -> ExceptT String IO ([SessionState], [SessionHandle], [(String, UserStateBinding)])
loadPostgresSessionImportSnapshot pool =
  withResourceMHandled
    (\err -> "Session startup import failed while connecting to Postgres: " ++ show err)
    pool
    (\conn -> do
      stateRows <- tryExcept
        (query_ conn "SELECT state_id::text, user_id, created_at, expires_at, idle_expires_at, revoked_at FROM session_states" :: IO [(String, String, UTCTime, UTCTime, UTCTime, Maybe UTCTime)])
        (\err -> "Session startup import failed while reading Postgres states: " ++ show err)
      handleRows <- tryExcept
        (query_ conn "SELECT session_id::text, state_id::text, issued_at, revoked_at FROM session_handles" :: IO [(String, String, UTCTime, Maybe UTCTime)])
        (\err -> "Session startup import failed while reading Postgres handles: " ++ show err)
      bindingRows <- tryExcept
        (query_ conn "SELECT user_id, state_id::text FROM session_user_bindings" :: IO [(String, String)])
        (\err -> "Session startup import failed while reading Postgres user bindings: " ++ show err)
      pure
        ( map (\(sid, uid, createdAt, expiresAt, idleExpiresAt, revokedAt) -> SessionState sid uid createdAt expiresAt idleExpiresAt revokedAt) stateRows
        , map (\(sessionId, stId, issuedAt, revokedAt) -> SessionHandle sessionId stId issuedAt revokedAt) handleRows
        , map (second UserStateBinding) bindingRows
        )
    )

decodeJsonDirectory :: FromJSON a => FilePath -> ExceptT String IO [a]
decodeJsonDirectory dirPath = do
  exists <- lift $ doesDirectoryExist dirPath
  if not exists
    then pure []
    else do
      files <- lift $ listDirectory dirPath
      foldM loadFile [] (sortOn id files)
  where
    loadFile acc fileName
      | takeExtension fileName /= ".json" = pure acc
      | otherwise = do
          let fullPath = dirPath </> fileName
          content <- lift $ BL.readFile fullPath
          case decode content of
            Nothing -> throwE ("invalid JSON in " ++ fullPath)
            Just parsed -> pure (parsed : acc)

decodeSessionBindingsDirectory :: FilePath -> ExceptT String IO [(String, UserStateBinding)]
decodeSessionBindingsDirectory dirPath = do
  exists <- lift $ doesDirectoryExist dirPath
  if not exists
    then pure []
    else do
      files <- lift $ listDirectory dirPath
      foldM loadFile [] (sortOn id files)
  where
    loadFile acc fileName
      | takeExtension fileName /= ".json" = pure acc
      | otherwise = do
          let fullPath = dirPath </> fileName
              userId = takeBaseName fileName
          content <- lift $ BL.readFile fullPath
          case decode content of
            Nothing -> throwE ("invalid JSON in " ++ fullPath)
            Just parsed -> pure ((userId, parsed) : acc)
