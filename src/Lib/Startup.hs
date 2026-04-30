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
import Data.Aeson (decode)
import Data.Int (Int64)
import Data.List (intercalate, nub, sort, sortOn)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as Ex
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.PostgreSQL.Simple (Connection, Only(..), close, connectPostgreSQL, execute, query, query_)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import qualified AgendaModel as Agenda
import Auth (AuthRepository, defaultAuthRepository)
import qualified AuthRepository
import CalendarRepository (CalendarRepository, defaultCalendarRepository, postgresCalendarRepository, calendarPostgresHealthChecks)
import Happstack.Server (Conf(..), askRq, nullConf, simpleHTTP)
import Lib.Config
import Lib.Server (apiController, homePage, log, serveStaticResource)
import qualified Lib.Startup.Import.Auth as StartupImportAuth
import qualified Lib.Startup.Import.Calendar as StartupImportCalendar
import qualified Lib.Startup.Import.NotesChecklist as StartupImportNotesChecklist
import qualified Lib.Startup.Import.Session as StartupImportSession
import qualified Lib.Startup.Import.TripSharing as StartupImportTripSharing
import NotesChecklistRepository (ChecklistRepository, NoteRepository, defaultChecklistRepository, defaultNoteRepository, postgresChecklistRepository, postgresNoteRepository, notePostgresHealthChecks, checklistPostgresHealthChecks)
import qualified Lib.Startup.Migrations as StartupMigrations
import Repository (RepositoryError(..))
import Session (SessionConfig(..), SessionHandle(..), SessionRepository(..), SessionState(..), SessionStore, UserStateBinding(..), mkFileSessionStore, mkPostgresSessionRepository, mkSessionStore, sessionPostgresHealthChecks)
import Helpers (tryExcept, withResourceMHandled)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)
import TripSharingRepository (TripSharingRepository, defaultTripSharingRepository, postgresTripSharingRepository, tripSharingPostgresHealthChecks)

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
      StartupMigrations.runStartupMigrations dbCfg
      sharedPool <- lift (createPostgresConnectionPool (renderPostgresConnectionString dbCfg))
      coreRepos <- wirePostgresCoreRepositories ctx sharedPool
      runPostgresStartupImports sharedPool (startupCd ctx)
      domainRepos <- wirePostgresDomainRepositories sharedPool
      lift (startHttpServer ctx coreRepos domainRepos)

prepareStartupContext :: ExceptT String IO StartupContext
prepareStartupContext = do
  appConfig <- loadAppConfigFromFile
  signupRateLimitState <- lift (newMVar [])
  tmpDir <- lift getTemporaryDirectory
  cd <- lift getCurrentDirectory
  pure StartupContext { startupSessionCfg = sessionConfig appConfig
                      , startupAppConfig = appConfig
                      , startupBackend = backend appConfig
                      , startupSignupRateLimitState = signupRateLimitState
                      , startupTmpDir = tmpDir
                      , startupCd = cd
                      }

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

wirePostgresCoreRepositories :: StartupContext -> Pool Connection -> ExceptT String IO CoreRepositories
wirePostgresCoreRepositories StartupContext {..} sharedPool = do
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

wirePostgresDomainRepositories :: Pool Connection -> ExceptT String IO DomainRepositories
wirePostgresDomainRepositories sharedPool = do
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
  StartupImportAuth.runAuthStartupImport pool
  StartupImportSession.runSessionStartupImport pool cd
  StartupImportCalendar.runCalendarStartupImport pool cd
  StartupImportTripSharing.runTripSharingStartupImport pool cd
  StartupImportNotesChecklist.runNoteStartupImport pool cd
  StartupImportNotesChecklist.runChecklistStartupImport pool cd

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
  verifyPostgresStorage "auth" pool AuthRepository.authPostgresHealthChecks
  pure (AuthRepository.postgresAuthRepository pool)


makePostgresCalendarRepository :: Pool Connection -> ExceptT String IO CalendarRepository
makePostgresCalendarRepository pool = do
  verifyPostgresStorage "calendar" pool calendarPostgresHealthChecks
  pure (postgresCalendarRepository pool)

makePostgresTripSharingRepository :: Pool Connection -> ExceptT String IO TripSharingRepository
makePostgresTripSharingRepository pool = do
  verifyPostgresStorage "trip-sharing" pool tripSharingPostgresHealthChecks
  pure (postgresTripSharingRepository pool)

makePostgresNoteRepository :: Pool Connection -> ExceptT String IO NoteRepository
makePostgresNoteRepository pool = do
  verifyPostgresStorage "note" pool notePostgresHealthChecks
  pure (postgresNoteRepository pool)

makePostgresChecklistRepository :: Pool Connection -> ExceptT String IO ChecklistRepository
makePostgresChecklistRepository pool = do
  verifyPostgresStorage "checklist" pool checklistPostgresHealthChecks
  pure (postgresChecklistRepository pool)

makePostgresSessionStore :: Pool Connection -> SessionConfig -> ExceptT String IO SessionStore
makePostgresSessionStore pool sessionCfg = do
  verifyPostgresStorage "session" pool sessionPostgresHealthChecks
  pure (mkSessionStore (mkPostgresSessionRepository pool) sessionCfg)

verifyPostgresStorage :: String -> Pool Connection -> (Connection -> ExceptT String IO ()) -> ExceptT String IO ()
verifyPostgresStorage domainName pool verifyActions = do
  handleE (\err -> throwE ("Postgres " ++ domainName ++ " storage validation failed: " ++ err)) $
    withResourceMHandled
      (\err -> "Unable to connect to Postgres: " ++ show err)
      pool
      (\conn -> do
        _ <- tryExcept (query_ conn "SELECT 1" :: IO [Only Int])
                       (\err -> "Postgres ping query failed: " ++ show err)
        verifyActions conn
      )
