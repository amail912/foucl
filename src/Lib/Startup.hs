{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Lib.Startup
  ( runApp
  , makePostgresSessionStore
  , makePostgresCalendarRepository
  , makePostgresTripSharingRepository
  , makePostgresNoteRepository
  , makePostgresChecklistRepository
  ) where

import Prelude hiding (log, writeFile)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad (foldM, msum, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, handleE, catchE)
import Data.Aeson (FromJSON, decode, encode)
import Data.Bifunctor (second)
import Data.Int (Int64)
import Data.List (intercalate, nub, sort, sortOn)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Control.Exception as Ex
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.PostgreSQL.Simple (Connection, Only(..), close, connectPostgreSQL, execute, query, query_)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import qualified AgendaModel as Agenda
import Auth (AuthRepository, defaultAuthRepository)
import qualified AuthRepository
import CalendarRepository (CalendarRepository, defaultCalendarRepository, postgresCalendarRepository, verifyPostgresCalendarStorage)
import Happstack.Server (Conf(..), askRq, nullConf, simpleHTTP)
import Lib.Config
import Lib.Server (apiController, homePage, log, serveStaticResource)
import Model (ChecklistContent, Identifiable(..), NoteContent)
import qualified Model
import NotesChecklistRepository (ChecklistRepository, NoteRepository, defaultChecklistRepository, defaultNoteRepository, postgresChecklistRepository, postgresNoteRepository, verifyPostgresChecklistStorage, verifyPostgresNoteStorage)
import PostgresMigrations (MigrationDirection(..), runAuthMigrationsAtPath, runCalendarMigrationsAtPath, runChecklistMigrationsAtPath, runNoteMigrationsAtPath, runSessionMigrationsAtPath, runTripSharingMigrationsAtPath)
import Repository (RepositoryError(..))
import Session (SessionConfig(..), SessionHandle(..), SessionRepository(..), SessionState(..), SessionStore, UserStateBinding(..), mkFileSessionStore, mkPostgresSessionRepository, mkSessionStore, verifyPostgresSessionStorage)
import Helpers (tryExcept, withResourceMHandled)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)
import TripSharingRepository (TripSharingRepository, defaultTripSharingRepository, postgresTripSharingRepository, verifyPostgresTripSharingStorage)

runApp :: IO ()
runApp = do
  putStrLn "running server"
  result <- runExceptT runAppStartup
  case result of
    Left err -> do
      putStrLn ("[startup-error] " ++ err)
      exitFailure
    Right () -> pure ()

data StartupContext = StartupContext
  { startupAppConfig :: !AppConfig
  , startupSessionCfg :: !SessionConfig
  , startupSignupRateLimitState :: !(MVar [UTCTime])
  , startupTmpDir :: !FilePath
  , startupCd :: !FilePath
  , startupBackend :: !Backend
  }

data CoreRepositories = CoreRepositories
  { coreSessionStore :: !SessionStore
  , coreAuthRepo :: !AuthRepository
  }

data DomainRepositories = DomainRepositories
  { domainCalendarRepo :: !CalendarRepository
  , domainTripSharingRepo :: !TripSharingRepository
  , domainNoteRepo :: !NoteRepository
  , domainChecklistRepo :: !ChecklistRepository
  }

runAppStartup :: ExceptT String IO ()
runAppStartup = do
  ctx <- prepareStartupContext
  lift (logStartupContext ctx)
  case startupBackend ctx of
    Filesystem -> do
      coreRepos <- wireFilesystemCoreRepositories ctx
      let domainRepos = DomainRepositories { domainCalendarRepo = defaultCalendarRepository
                                           , domainTripSharingRepo = defaultTripSharingRepository
                                           , domainNoteRepo = defaultNoteRepository
                                           , domainChecklistRepo = defaultChecklistRepository
                                           }
      lift (startHttpServer ctx coreRepos domainRepos)
    Postgres -> do
      dbCfg <- maybe (throwE "Configuration database is required when backend=postgres") pure (databaseConfig (startupAppConfig ctx))
      runStartupMigrations dbCfg
      sharedPool <- lift (createPostgresConnectionPool (renderPostgresConnectionString dbCfg))
      coreRepos <- wirePostgresCoreRepositories ctx dbCfg sharedPool
      runPostgresStartupImports sharedPool (startupCd ctx)
      domainRepos <- wirePostgresDomainRepositories dbCfg sharedPool
      lift (startHttpServer ctx coreRepos domainRepos)

prepareStartupContext :: ExceptT String IO StartupContext
prepareStartupContext = do
  appConfig <- loadAppConfigFromFile
  signupRateLimitState <- lift (newMVar [])
  tmpDir <- lift getTemporaryDirectory
  cd <- lift getCurrentDirectory
  let startupSessionCfg = sessionConfig appConfig
      startupAppConfig = appConfig
      startupBackend = backend appConfig
      startupSignupRateLimitState = signupRateLimitState
      startupTmpDir = tmpDir
      startupCd = cd
  pure StartupContext {..}

logStartupContext :: StartupContext -> IO ()
logStartupContext StartupContext {..} = do
  putStrLn ("[startup] backend: " ++ show startupBackend)
  case databaseConfig startupAppConfig of
    Nothing -> pure ()
    Just dbCfg -> putStrLn ("[startup] database target: " ++ renderDatabaseTarget dbCfg)

wireFilesystemCoreRepositories :: StartupContext -> ExceptT String IO CoreRepositories
wireFilesystemCoreRepositories StartupContext {..} = do
  coreSessionStore <- catchE
    (lift $ mkFileSessionStore (startupCd </> "data" </> "sessions") startupSessionCfg)
    (\err -> do
      lift $ putStrLn "[startup] session backend wiring failed for: filesystem"
      throwE err)
  lift $ putStrLn "[startup] session backend wiring ready: filesystem"
  lift $ putStrLn "[startup] auth backend wiring ready: filesystem"
  pure CoreRepositories { coreSessionStore = coreSessionStore, coreAuthRepo = defaultAuthRepository }

wirePostgresCoreRepositories :: StartupContext -> DatabaseConfig -> Pool Connection -> ExceptT String IO CoreRepositories
wirePostgresCoreRepositories StartupContext {..} dbCfg sharedPool = do
  coreSessionStore <- catchE
    (makePostgresSessionStore sharedPool startupSessionCfg)
    (\err -> do
      lift $ putStrLn "[startup] session backend wiring failed for: postgres"
      throwE err)
  lift $ putStrLn "[startup] session backend wiring ready: postgres"
  coreAuthRepo <- catchE
    (makePostgresAuthRepository sharedPool)
    (\err -> do
      lift $ putStrLn "[startup] auth backend wiring failed for: postgres"
      throwE err)
  lift $ putStrLn "[startup] auth backend wiring ready: postgres"
  pure CoreRepositories {..}

wirePostgresDomainRepositories :: DatabaseConfig -> Pool Connection -> ExceptT String IO DomainRepositories
wirePostgresDomainRepositories _dbCfg sharedPool = do
  domainCalendarRepo <- catchE (makePostgresCalendarRepository sharedPool)
                               (\err -> do
                                 lift $ putStrLn "[startup] calendar backend wiring failed for: postgres"
                                 throwE err)
  lift $ putStrLn "[startup] calendar backend wiring ready: postgres"
  domainTripSharingRepo <- catchE (makePostgresTripSharingRepository sharedPool)
                                  (\err -> do
                                    lift $ putStrLn "[startup] trip-sharing backend wiring failed for: postgres"
                                    throwE err)
  lift $ putStrLn "[startup] trip-sharing backend wiring ready: postgres"
  domainNoteRepo <- catchE (makePostgresNoteRepository sharedPool)
                           (\err -> do
                             lift $ putStrLn "[startup] note backend wiring failed for: postgres"
                             throwE err)
  lift $ putStrLn "[startup] note backend wiring ready: postgres"
  domainChecklistRepo <- catchE (makePostgresChecklistRepository sharedPool)
                                (\err -> do
                                  lift $ putStrLn "[startup] checklist backend wiring failed for: postgres"
                                  throwE err)
  lift $ putStrLn "[startup] checklist backend wiring ready: postgres"
  pure DomainRepositories {..}

runPostgresStartupImports :: Pool Connection -> FilePath -> ExceptT String IO ()
runPostgresStartupImports pool cd = do
  runAuthStartupImport pool
  runSessionStartupImport pool cd
  runCalendarStartupImport pool cd
  runTripSharingStartupImport pool cd
  runNoteStartupImport pool cd
  runChecklistStartupImport pool cd

startHttpServer :: StartupContext -> CoreRepositories -> DomainRepositories -> IO ()
startHttpServer StartupContext {..} CoreRepositories {..} DomainRepositories {..} =
  simpleHTTP nullConf { port = 8081 } $ do
    rq <- askRq
    response <-
      msum
        [ homePage
        , apiController
            coreAuthRepo
            domainCalendarRepo
            domainTripSharingRepo
            domainNoteRepo
            domainChecklistRepo
            startupSignupRateLimitState
            startupTmpDir
            startupAppConfig
            coreSessionStore
        , serveStaticResource
        , mzero
        ]
    log "=========================END REQUEST====================\n"
    pure response

withErrPrefix :: String -> ExceptT String IO a -> ExceptT String IO a
withErrPrefix prefix action =
  catchE action (\err -> throwE (prefix ++ err))

runStartupMigrations
  :: DatabaseConfig
  -> ExceptT String IO ()
runStartupMigrations dbCfg = do
  let selectedDomains = ["auth", "session", "calendar", "trip-sharing", "note", "checklist"]
      selectedRendered = intercalate "," selectedDomains
      selectedCount = length selectedDomains
  lift $ putStrLn
    ( "[startup][migrations] start"
        ++ " direction=MigrateUp"
        ++ " selected="
        ++ selectedRendered
    )
  let connectionString = renderPostgresConnectionString dbCfg
  runDomains connectionString selectedDomains selectedRendered selectedCount
  where
    runDomains _ [] selectedRendered selectedCount = do
      lift $ putStrLn
        ( "[startup][migrations] completed"
            ++ " direction=MigrateUp"
            ++ " domains="
            ++ show selectedCount
            ++ " selected="
            ++ selectedRendered
        )
    runDomains connectionString (domain:rest) selectedRendered selectedCount = do
      lift $ putStrLn ("[startup][migrations] domain=" ++ domain ++ " phase=start direction=MigrateUp")
      catchE (runDomainMigration "." connectionString domain)
             (\err -> do
                 lift $ putStrLn
                     ( "[startup][migrations] failed"
                         ++ " direction=MigrateUp"
                         ++ " domain="
                         ++ domain
                         ++ " selected="
                         ++ selectedRendered
                         ++ " reason="
                         ++ err
                     )
                 throwE err)
      lift $ putStrLn ("[startup][migrations] domain=" ++ domain ++ " phase=done direction=MigrateUp")
      runDomains connectionString rest selectedRendered selectedCount

runDomainMigration :: FilePath -> String -> String -> ExceptT String IO ()
runDomainMigration basePath connectionString domain = do
  handleE (\err -> throwE $ "Startup migrations failed for domain=" ++ domain ++ ": " ++ err)
          (case domain of
            "auth" -> runAuthMigrationsAtPath basePath connectionString MigrateUp
            "session" -> runSessionMigrationsAtPath basePath connectionString MigrateUp
            "calendar" -> runCalendarMigrationsAtPath basePath connectionString MigrateUp
            "trip-sharing" -> runTripSharingMigrationsAtPath basePath connectionString MigrateUp
            "note" -> runNoteMigrationsAtPath basePath connectionString MigrateUp
            "checklist" -> runChecklistMigrationsAtPath basePath connectionString MigrateUp
            _ -> throwE $ "Unsupported migration domain: " <> domain)
  pure ()

createPostgresConnectionPool :: String -> IO (Pool Connection)
createPostgresConnectionPool connectionString =
  newPool $
    defaultPoolConfig
      (connectPostgreSQL (BS8.pack connectionString))
      close
      60
      16

makePostgresAuthRepository :: Pool Connection -> ExceptT String IO AuthRepository
makePostgresAuthRepository pool = do
  handleE (\err -> throwE ("Postgres auth storage validation failed: " ++ err))
          (AuthRepository.verifyPostgresAuthStorage pool)
  pure $ AuthRepository.postgresAuthRepository pool

runAuthStartupImport :: Pool Connection -> ExceptT String IO ()
runAuthStartupImport pool = do
  let postgresRepo = AuthRepository.postgresAuthRepository pool
  fsUsers <- catchE (AuthRepository.repoListUsers  defaultAuthRepository)
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

runNoteStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runNoteStartupImport pool cd = do
  let notesBaseDir = cd </> "data" </> "note"
  fsEntries <- loadFilesystemNoteImportEntries notesBaseDir
  (importedCount, skippedCount, totalPgRows) <-
    withResourceMHandled
      (\err -> "Note startup import failed while connecting to Postgres: " ++ show err)
      pool
      (\conn -> do
        pgRows <- tryExcept (query_ conn "SELECT item_id FROM note_items" :: IO [Only String])
                            (\err -> "Note startup import failed while reading Postgres notes: " ++ show err)
        let orderedFsEntries = sortOn (\(itemId, _, _) -> itemId) fsEntries
            knownIds = Set.fromList (map fromOnly pgRows)
        when (not (null orderedFsEntries) && not (null pgRows)) $
          lift $ putStrLn
            ( "[startup][note-import][warning] overlap detected:"
                ++ " filesystem_count="
                ++ show (length orderedFsEntries)
                ++ " postgres_count="
                ++ show (length pgRows)
                ++ " conflict_policy=postgres-wins"
            )
        (i, s, _) <- foldM (importSingleNoteItem conn) (0 :: Int, 0 :: Int, knownIds) orderedFsEntries
        pure (i, s, length pgRows)
      )
  lift $ putStrLn
    ( "[startup][note-import] completed"
        ++ " filesystem_count="
        ++ show (length fsEntries)
        ++ " postgres_count="
        ++ show totalPgRows
        ++ " imported="
        ++ show importedCount
        ++ " skipped_conflicts="
        ++ show skippedCount
    )

runChecklistStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runChecklistStartupImport pool cd = do
  let checklistsBaseDir = cd </> "data" </> "checklist"
  fsEntries <- loadFilesystemChecklistImportEntries checklistsBaseDir
  (importedCount, skippedCount, totalPgRows) <-
    withResourceMHandled
      (\err -> "Checklist startup import failed while connecting to Postgres: " ++ show err)
      pool
      (\conn -> do
        pgRows <- tryExcept (query_ conn "SELECT item_id FROM checklist_items" :: IO [Only String])
                            (\err -> "Checklist startup import failed while reading Postgres checklists: " ++ show err)
        let orderedFsEntries = sortOn (\(itemId, _, _) -> itemId) fsEntries
            knownIds = Set.fromList (map fromOnly pgRows)
        when (not (null orderedFsEntries) && not (null pgRows)) $
          lift $ putStrLn
            ( "[startup][checklist-import][warning] overlap detected:"
                ++ " filesystem_count="
                ++ show (length orderedFsEntries)
                ++ " postgres_count="
                ++ show (length pgRows)
                ++ " conflict_policy=postgres-wins"
            )
        (i, s, _) <- foldM (importSingleChecklistItem conn) (0 :: Int, 0 :: Int, knownIds) orderedFsEntries
        pure (i, s, length pgRows)
      )
  lift $ putStrLn
    ( "[startup][checklist-import] completed"
        ++ " filesystem_count="
        ++ show (length fsEntries)
        ++ " postgres_count="
        ++ show totalPgRows
        ++ " imported="
        ++ show importedCount
        ++ " skipped_conflicts="
        ++ show skippedCount
    )

importSingleNoteItem
  :: Connection
  -> (Int, Int, Set.Set String)
  -> (String, String, NoteContent)
  -> ExceptT String IO (Int, Int, Set.Set String)
importSingleNoteItem conn (importedCount, skippedCount, knownIds) (itemId, itemVersion, noteContent) =
  if Set.member itemId knownIds
    then do
      lift $ putStrLn ("[startup][note-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownIds)
    else do
      affected <- tryExcept (execute conn
                              "INSERT INTO note_items (item_id, item_version, item_content) VALUES (?, ?, ?::jsonb) ON CONFLICT (item_id) DO NOTHING"
                              (itemId, itemVersion, BL8.unpack (encode noteContent)))
                            (\err -> "Note startup import failed while writing item_id=" ++ itemId ++ ": " ++ show err)
      if affected > 0
        then
          pure (importedCount + 1, skippedCount, Set.insert itemId knownIds)
        else do
          lift $ putStrLn ("[startup][note-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert itemId knownIds)

importSingleChecklistItem
  :: Connection
  -> (Int, Int, Set.Set String)
  -> (String, String, ChecklistContent)
  -> ExceptT String IO (Int, Int, Set.Set String)
importSingleChecklistItem conn (importedCount, skippedCount, knownIds) (itemId, itemVersion, checklistContent) =
  if Set.member itemId knownIds
    then do
      lift $ putStrLn ("[startup][checklist-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownIds)
    else do
      affected <- tryExcept (execute conn
                              "INSERT INTO checklist_items (item_id, item_version, item_content) VALUES (?, ?, ?::jsonb) ON CONFLICT (item_id) DO NOTHING"
                              (itemId, itemVersion, BL8.unpack (encode checklistContent)))
                            (\err -> "Checklist startup import failed while writing item_id=" ++ itemId ++ ": " ++ show err)
      if affected > 0
        then
          pure (importedCount + 1, skippedCount, Set.insert itemId knownIds)
        else do
          lift $ putStrLn ("[startup][checklist-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert itemId knownIds)

loadFilesystemNoteImportEntries :: FilePath -> ExceptT String IO [(String, String, NoteContent)]
loadFilesystemNoteImportEntries rootDir = do
  exists <- lift $ doesDirectoryExist rootDir
  if not exists
    then do
      lift $ putStrLn "[startup][note-import] source note directory is missing; treating filesystem note source as empty"
      pure []
    else do
      files <- lift $ listDirectory rootDir
      foldM (decodeSingleFilesystemNote rootDir) [] (sort files)

decodeSingleFilesystemNote
  :: FilePath
  -> [(String, String, NoteContent)]
  -> FilePath
  -> ExceptT String IO [(String, String, NoteContent)]
decodeSingleFilesystemNote rootDir acc fileName
  | takeExtension fileName /= ".txt" = pure acc
  | otherwise = do
      let fullPath = rootDir </> fileName
      contentResult <- lift (Ex.try (BL.readFile fullPath) :: IO (Either Ex.IOException BL.ByteString))
      case contentResult of
        Left err ->
          throwE ("Note startup import failed while reading filesystem note file " ++ fullPath ++ ": " ++ show err)
        Right raw ->
          case decode raw of
            Nothing ->
              throwE ("Note startup import failed while reading filesystem notes: invalid JSON in " ++ fullPath)
            Just noteItem ->
              let itemStorageId = storageId (noteItem :: Identifiable NoteContent)
                  itemId = Model.id itemStorageId
                  itemVersion = Model.version itemStorageId
               in pure ((itemId, itemVersion, content noteItem) : acc)

loadFilesystemChecklistImportEntries :: FilePath -> ExceptT String IO [(String, String, ChecklistContent)]
loadFilesystemChecklistImportEntries rootDir = do
  exists <- lift $ doesDirectoryExist rootDir
  if not exists
    then do
      lift $ putStrLn "[startup][checklist-import] source checklist directory is missing; treating filesystem checklist source as empty"
      pure []
    else do
      files <- lift $ listDirectory rootDir
      foldM (decodeSingleFilesystemChecklist rootDir) [] (sort files)

decodeSingleFilesystemChecklist
  :: FilePath
  -> [(String, String, ChecklistContent)]
  -> FilePath
  -> ExceptT String IO [(String, String, ChecklistContent)]
decodeSingleFilesystemChecklist rootDir acc fileName
  | takeExtension fileName /= ".txt" = pure acc
  | otherwise = do
      let fullPath = rootDir </> fileName
      contentResult <- lift (Ex.try (BL.readFile fullPath) :: IO (Either Ex.IOException BL.ByteString))
      case contentResult of
        Left err ->
          throwE ("Checklist startup import failed while reading filesystem checklist file " ++ fullPath ++ ": " ++ show err)
        Right raw ->
          case decode raw of
            Nothing ->
              throwE ("Checklist startup import failed while reading filesystem checklists: invalid JSON in " ++ fullPath)
            Just checklistItem ->
              let itemStorageId = storageId (checklistItem :: Identifiable ChecklistContent)
                  itemId = Model.id itemStorageId
                  itemVersion = Model.version itemStorageId
               in pure ((itemId, itemVersion, content checklistItem) : acc)

runCalendarStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runCalendarStartupImport pool cd = do
  let calendarBaseDir = cd </> "data" </> "calendar-items"
  fsEntries <- loadFilesystemCalendarImportEntries calendarBaseDir
  (importedCount, skippedCount, totalPgRows) <-
    withResourceMHandled
      (\err -> "Calendar startup import failed while connecting to Postgres: " ++ show err)
      pool
      (\conn -> do
        pgRows <- tryExcept (query_ conn "SELECT user_id, item_id FROM calendar_items" :: IO [(String, String)])
                            (\err -> "Calendar startup import failed while reading Postgres items: " ++ show err)
        let orderedFsEntries = sortOn (\(userId, itemId, _) -> (userId, itemId)) fsEntries
            knownKeys = Set.fromList pgRows
        when (not (null orderedFsEntries) && not (null pgRows)) $
          lift $ putStrLn
            ( "[startup][calendar-import][warning] overlap detected:"
                ++ " filesystem_count="
                ++ show (length orderedFsEntries)
                ++ " postgres_count="
                ++ show (length pgRows)
                ++ " conflict_policy=postgres-wins"
            )
        (i, s, _) <- foldM (importSingleCalendarItem conn) (0 :: Int, 0 :: Int, knownKeys) orderedFsEntries
        pure (i, s, length pgRows)
      )
  lift $ putStrLn
    ( "[startup][calendar-import] completed"
        ++ " filesystem_count="
        ++ show (length fsEntries)
        ++ " postgres_count="
        ++ show totalPgRows
        ++ " imported="
        ++ show importedCount
        ++ " skipped_conflicts="
        ++ show skippedCount
    )

importSingleCalendarItem
  :: Connection
  -> (Int, Int, Set.Set (String, String))
  -> (String, String, Agenda.CalendarItemContent)
  -> ExceptT String IO (Int, Int, Set.Set (String, String))
importSingleCalendarItem conn (importedCount, skippedCount, knownKeys) (userId, itemId, content) = do
  let key = (userId, itemId)
  if Set.member key knownKeys
    then do
      lift $ putStrLn ("[startup][calendar-import][warning] skipping conflicting user_id=" ++ userId ++ " item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownKeys)
    else do
      let row = calendarContentToDbRow content
      affected <- tryExcept
        (execute conn
          "INSERT INTO calendar_items (user_id, item_id, item_kind, item_type, title, window_start, window_end, status, source_item_id, actual_duration_minutes, category, recurrence_rule_type, recurrence_interval_days, recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (user_id, item_id) DO NOTHING"
          ( userId
          , itemId
          , dbCalendarItemKind row
          , dbCalendarLegacyItemType row
          , dbCalendarLegacyTitle row
          , dbCalendarLegacyWindowStart row
          , dbCalendarLegacyWindowEnd row
          , dbCalendarLegacyStatus row
          , dbCalendarLegacySourceItemId row
          , dbCalendarLegacyActualDurationMinutes row
          , dbCalendarLegacyCategory row
          , dbCalendarLegacyRecurrenceRuleType row
          , dbCalendarLegacyRecurrenceIntervalDays row
          , dbCalendarLegacyRecurrenceExceptionDates row
          , dbCalendarTripWindowStart row
          , dbCalendarTripWindowEnd row
          , dbCalendarTripDeparturePlaceId row
          , dbCalendarTripArrivalPlaceId row
          ))
        (\err -> "Calendar startup import failed while writing user_id=" ++ userId ++ " item_id=" ++ itemId ++ ": " ++ show err)
      if affected > 0
        then pure (importedCount + 1, skippedCount, Set.insert key knownKeys)
        else do
          lift $ putStrLn ("[startup][calendar-import][warning] skipping conflicting user_id=" ++ userId ++ " item_id=" ++ itemId ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert key knownKeys)

loadFilesystemCalendarImportEntries :: FilePath -> ExceptT String IO [(String, String, Agenda.CalendarItemContent)]
loadFilesystemCalendarImportEntries calendarBaseDir = do
  baseExists <- lift $ doesDirectoryExist calendarBaseDir
  if not baseExists
    then do
      lift $ putStrLn "[startup][calendar-import] source calendar directory is missing; treating filesystem calendar source as empty"
      pure []
    else do
      entries <- lift $ listDirectory calendarBaseDir
      foldM (loadSingleCalendarUserDirectory calendarBaseDir) [] (sort entries)

loadSingleCalendarUserDirectory
  :: FilePath
  -> [(String, String, Agenda.CalendarItemContent)]
  -> FilePath
  -> ExceptT String IO [(String, String, Agenda.CalendarItemContent)]
loadSingleCalendarUserDirectory calendarBaseDir acc userId = do
  let userDir = calendarBaseDir </> userId
  isDir <- lift $ doesDirectoryExist userDir
  if not isDir
    then pure acc
    else do
      items <- withErrPrefix ("Calendar startup import failed while reading filesystem items for user_id=" ++ userId ++ ": ") (decodeJsonDirectory userDir :: ExceptT String IO [Agenda.CalendarItem])
      foldM (extractCalendarImportEntry userId) acc items

extractCalendarImportEntry
  :: String
  -> [(String, String, Agenda.CalendarItemContent)]
  -> Agenda.CalendarItem
  -> ExceptT String IO [(String, String, Agenda.CalendarItemContent)]
extractCalendarImportEntry userId acc item =
  case item of
    Agenda.ServerCalendarItem {Agenda.itemId, Agenda.content} ->
      pure ((userId, itemId, content) : acc)
    Agenda.NewCalendarItem {} ->
      throwE ("Calendar startup import failed while reading filesystem items for user_id=" ++ userId ++ ": expected stored calendar item with id")

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

data CalendarImportDbRow = CalendarImportDbRow
  { dbCalendarItemKind :: String
  , dbCalendarLegacyItemType :: Maybe String
  , dbCalendarLegacyTitle :: Maybe String
  , dbCalendarLegacyWindowStart :: Maybe String
  , dbCalendarLegacyWindowEnd :: Maybe String
  , dbCalendarLegacyStatus :: Maybe String
  , dbCalendarLegacySourceItemId :: Maybe String
  , dbCalendarLegacyActualDurationMinutes :: Maybe Int
  , dbCalendarLegacyCategory :: Maybe String
  , dbCalendarLegacyRecurrenceRuleType :: Maybe String
  , dbCalendarLegacyRecurrenceIntervalDays :: Maybe Int
  , dbCalendarLegacyRecurrenceExceptionDates :: PGArray String
  , dbCalendarTripWindowStart :: Maybe String
  , dbCalendarTripWindowEnd :: Maybe String
  , dbCalendarTripDeparturePlaceId :: Maybe String
  , dbCalendarTripArrivalPlaceId :: Maybe String
  }

calendarContentToDbRow :: Agenda.CalendarItemContent -> CalendarImportDbRow
calendarContentToDbRow content =
  case content of
    Agenda.CalendarItemContent
      { Agenda.itemType
      , Agenda.title
      , Agenda.windowStart
      , Agenda.windowEnd
      , Agenda.status
      , Agenda.sourceItemId
      , Agenda.actualDurationMinutes
      , Agenda.category
      , Agenda.recurrenceRule
      , Agenda.recurrenceExceptionDates
      } ->
        CalendarImportDbRow
          { dbCalendarItemKind = "task"
          , dbCalendarLegacyItemType = Just (calendarItemTypeToDb itemType)
          , dbCalendarLegacyTitle = Just title
          , dbCalendarLegacyWindowStart = Just windowStart
          , dbCalendarLegacyWindowEnd = Just windowEnd
          , dbCalendarLegacyStatus = Just (calendarItemStatusToDb status)
          , dbCalendarLegacySourceItemId = sourceItemId
          , dbCalendarLegacyActualDurationMinutes = actualDurationMinutes
          , dbCalendarLegacyCategory = category
          , dbCalendarLegacyRecurrenceRuleType = fst (calendarRecurrenceToDb recurrenceRule)
          , dbCalendarLegacyRecurrenceIntervalDays = snd (calendarRecurrenceToDb recurrenceRule)
          , dbCalendarLegacyRecurrenceExceptionDates = PGArray recurrenceExceptionDates
          , dbCalendarTripWindowStart = Nothing
          , dbCalendarTripWindowEnd = Nothing
          , dbCalendarTripDeparturePlaceId = Nothing
          , dbCalendarTripArrivalPlaceId = Nothing
          }
    Agenda.TripCalendarItemContent Agenda.TripItemContent {Agenda.tripWindowStart, Agenda.tripWindowEnd, Agenda.departurePlaceId, Agenda.arrivalPlaceId} ->
      CalendarImportDbRow
        { dbCalendarItemKind = "trip"
        , dbCalendarLegacyItemType = Nothing
        , dbCalendarLegacyTitle = Nothing
        , dbCalendarLegacyWindowStart = Nothing
        , dbCalendarLegacyWindowEnd = Nothing
        , dbCalendarLegacyStatus = Nothing
        , dbCalendarLegacySourceItemId = Nothing
        , dbCalendarLegacyActualDurationMinutes = Nothing
        , dbCalendarLegacyCategory = Nothing
        , dbCalendarLegacyRecurrenceRuleType = Nothing
        , dbCalendarLegacyRecurrenceIntervalDays = Nothing
        , dbCalendarLegacyRecurrenceExceptionDates = PGArray []
        , dbCalendarTripWindowStart = Just tripWindowStart
        , dbCalendarTripWindowEnd = Just tripWindowEnd
        , dbCalendarTripDeparturePlaceId = Just departurePlaceId
        , dbCalendarTripArrivalPlaceId = Just arrivalPlaceId
        }

calendarItemTypeToDb :: Agenda.ItemType -> String
calendarItemTypeToDb Agenda.Intention = "INTENTION"
calendarItemTypeToDb Agenda.ScheduledBlock = "BLOC_PLANIFIE"

calendarItemStatusToDb :: Agenda.ItemStatus -> String
calendarItemStatusToDb Agenda.Todo = "TODO"
calendarItemStatusToDb Agenda.EnCours = "EN_COURS"
calendarItemStatusToDb Agenda.Fait = "FAIT"
calendarItemStatusToDb Agenda.Annule = "ANNULE"

calendarRecurrenceToDb :: Maybe Agenda.RecurrenceRule -> (Maybe String, Maybe Int)
calendarRecurrenceToDb Nothing = (Nothing, Nothing)
calendarRecurrenceToDb (Just recurrenceRule) =
  case recurrenceRule of
    Agenda.RecurrenceDaily -> (Just "DAILY", Nothing)
    Agenda.RecurrenceWeekly -> (Just "WEEKLY", Nothing)
    Agenda.RecurrenceMonthly -> (Just "MONTHLY", Nothing)
    Agenda.RecurrenceYearly -> (Just "YEARLY", Nothing)
    Agenda.RecurrenceEveryXDays intervalDays -> (Just "EVERY_X_DAYS", Just intervalDays)

makePostgresCalendarRepository :: Pool Connection -> ExceptT String IO CalendarRepository
makePostgresCalendarRepository pool = do
  handleE (\err -> throwE ("Postgres calendar storage validation failed: " ++ err))
          (verifyPostgresCalendarStorage pool)
  pure $ postgresCalendarRepository pool

makePostgresTripSharingRepository :: Pool Connection -> ExceptT String IO TripSharingRepository
makePostgresTripSharingRepository pool = do
  handleE (\err -> throwE ("Postgres trip-sharing storage validation failed: " ++ err))
          (verifyPostgresTripSharingStorage pool)
  pure $ postgresTripSharingRepository pool

makePostgresNoteRepository :: Pool Connection -> ExceptT String IO NoteRepository
makePostgresNoteRepository pool = do
  handleE (\err -> throwE ("Postgres note storage validation failed: " ++ err))
          (verifyPostgresNoteStorage pool)
  pure $ postgresNoteRepository pool

makePostgresChecklistRepository :: Pool Connection -> ExceptT String IO ChecklistRepository
makePostgresChecklistRepository pool = do
  handleE (\err -> throwE ("Postgres checklist storage validation failed: " ++ err))
          (verifyPostgresChecklistStorage pool)
  pure $ postgresChecklistRepository pool

makePostgresSessionStore :: Pool Connection -> SessionConfig -> ExceptT String IO SessionStore
makePostgresSessionStore pool sessionCfg = do
  handleE (\err -> throwE ("Postgres session storage validation failed: " ++ err))
          (verifyPostgresSessionStorage pool)
  pure $ mkSessionStore (mkPostgresSessionRepository pool) sessionCfg
