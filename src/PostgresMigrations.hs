module PostgresMigrations
  ( MigrationDirection(..)
  , runAuthMigrations
  , runAuthMigrationsAtPath
  , runSessionMigrations
  , runSessionMigrationsAtPath
  , runCalendarMigrations
  , runCalendarMigrationsAtPath
  , runTripSharingMigrations
  , runTripSharingMigrationsAtPath
  , runFinanceMigrations
  , runFinanceMigrationsAtPath
  , runNoteMigrations
  , runNoteMigrationsAtPath
  , runChecklistMigrations
  , runChecklistMigrationsAtPath
  , psqlAvailable
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, handleE)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

data MigrationDirection
  = MigrateUp
  | MigrateDown
  deriving (Eq, Show)

data SqlMigration = SqlMigration
  { migrationId :: !String
  , upSqlPath :: !FilePath
  , downSqlPath :: !FilePath
  }

data MigrationStats = MigrationStats
  { statsApplied :: !Int
  , statsSkipped :: !Int
  }

runAuthMigrations :: String -> MigrationDirection -> ExceptT String IO ()
runAuthMigrations = runAuthMigrationsAtPath "."

runAuthMigrationsAtPath :: FilePath -> String -> MigrationDirection -> ExceptT String IO ()
runAuthMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "auth" basePath connectionUrl direction authMigrations

runSessionMigrations :: String -> MigrationDirection -> ExceptT String IO ()
runSessionMigrations = runSessionMigrationsAtPath "."

runSessionMigrationsAtPath :: FilePath -> String -> MigrationDirection -> ExceptT String IO ()
runSessionMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "session" basePath connectionUrl direction sessionMigrations

runCalendarMigrations :: String -> MigrationDirection -> ExceptT String IO ()
runCalendarMigrations = runCalendarMigrationsAtPath "."

runCalendarMigrationsAtPath :: FilePath -> String -> MigrationDirection -> ExceptT String IO ()
runCalendarMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "calendar" basePath connectionUrl direction calendarMigrations

runTripSharingMigrations :: String -> MigrationDirection -> ExceptT String IO ()
runTripSharingMigrations = runTripSharingMigrationsAtPath "."

runTripSharingMigrationsAtPath :: FilePath -> String -> MigrationDirection -> ExceptT String IO ()
runTripSharingMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "trip-sharing" basePath connectionUrl direction tripSharingMigrations

runFinanceMigrations :: String -> MigrationDirection -> ExceptT String IO ()
runFinanceMigrations = runFinanceMigrationsAtPath "."

runFinanceMigrationsAtPath :: FilePath -> String -> MigrationDirection -> ExceptT String IO ()
runFinanceMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "finance" basePath connectionUrl direction financeMigrations

runNoteMigrations :: String -> MigrationDirection -> ExceptT String IO ()
runNoteMigrations = runNoteMigrationsAtPath "."

runNoteMigrationsAtPath :: FilePath -> String -> MigrationDirection -> ExceptT String IO ()
runNoteMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "note" basePath connectionUrl direction noteMigrations

runChecklistMigrations :: String -> MigrationDirection -> ExceptT String IO ()
runChecklistMigrations = runChecklistMigrationsAtPath "."

runChecklistMigrationsAtPath :: FilePath -> String -> MigrationDirection -> ExceptT String IO ()
runChecklistMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "checklist" basePath connectionUrl direction checklistMigrations

runMigrationsAtPath :: String -> FilePath -> String -> MigrationDirection -> [SqlMigration] -> ExceptT String IO ()
runMigrationsAtPath domain basePath connectionUrl direction migrations = do
  startedAt <- lift getCurrentTime
  lift $ putStrLn ("[migrations] starting " ++ domain ++ " migrations direction=" ++ show direction)
  available <- lift psqlAvailable
  if not available
    then throwE "psql binary not found in PATH"
    else do
      ensureResult <- ensureMigrationsTable connectionUrl
      stats <- handleE (\err -> do
                          endedAt <- lift getCurrentTime
                          let elapsed = diffUTCTime endedAt startedAt
                          lift $ putStrLn ("[migrations] " ++ domain ++ " migrations failed direction=" ++ show direction ++ " elapsed=" ++ show elapsed)
                          throwE err) $ case direction of
                                          MigrateUp -> applyAllUp basePath connectionUrl migrations
                                          MigrateDown -> applyAllDown basePath connectionUrl (reverse migrations)
      endedAt <- lift getCurrentTime
      let elapsed = diffUTCTime endedAt startedAt
      lift $ putStrLn ("[migrations] " ++ domain ++ " migrations completed direction=" ++ show direction ++ " applied=" ++ show (statsApplied stats) ++ " skipped=" ++ show (statsSkipped stats) ++ " elapsed=" ++ show elapsed)
      pure()

psqlAvailable :: IO Bool
psqlAvailable = do
  mPath <- findExecutable "psql"
  pure (isJust mPath)

authMigrations :: [SqlMigration]
authMigrations =
  [ SqlMigration
      { migrationId = "0001_auth_schema"
      , upSqlPath = "db/migrations/auth/0001_auth_schema.up.sql"
      , downSqlPath = "db/migrations/auth/0001_auth_schema.down.sql"
      }
  ]

sessionMigrations :: [SqlMigration]
sessionMigrations =
  [ SqlMigration
      { migrationId = "0001_session_schema"
      , upSqlPath = "db/migrations/session/0001_session_schema.up.sql"
      , downSqlPath = "db/migrations/session/0001_session_schema.down.sql"
      }
  ]

calendarMigrations :: [SqlMigration]
calendarMigrations =
  [ SqlMigration
      { migrationId = "0001_calendar_schema"
      , upSqlPath = "db/migrations/calendar/0001_calendar_schema.up.sql"
      , downSqlPath = "db/migrations/calendar/0001_calendar_schema.down.sql"
      }
  ]

tripSharingMigrations :: [SqlMigration]
tripSharingMigrations =
  [ SqlMigration
      { migrationId = "0001_trip_sharing_schema"
      , upSqlPath = "db/migrations/trip-sharing/0001_trip_sharing_schema.up.sql"
      , downSqlPath = "db/migrations/trip-sharing/0001_trip_sharing_schema.down.sql"
      }
  ]

financeMigrations :: [SqlMigration]
financeMigrations =
  [ SqlMigration
      { migrationId = "0001_finance_schema"
      , upSqlPath = "db/migrations/finance/0001_finance_schema.up.sql"
      , downSqlPath = "db/migrations/finance/0001_finance_schema.down.sql"
      }
  , SqlMigration
      { migrationId = "0002_finance_transactions"
      , upSqlPath = "db/migrations/finance/0002_finance_transactions.up.sql"
      , downSqlPath = "db/migrations/finance/0002_finance_transactions.down.sql"
      }
  , SqlMigration
      { migrationId = "0003_finance_categories"
      , upSqlPath = "db/migrations/finance/0003_finance_categories.up.sql"
      , downSqlPath = "db/migrations/finance/0003_finance_categories.down.sql"
      }
  , SqlMigration
      { migrationId = "0004_finance_transaction_classification"
      , upSqlPath = "db/migrations/finance/0004_finance_transaction_classification.up.sql"
      , downSqlPath = "db/migrations/finance/0004_finance_transaction_classification.down.sql"
      }
  , SqlMigration
      { migrationId = "0005_finance_transaction_links"
      , upSqlPath = "db/migrations/finance/0005_finance_transaction_links.up.sql"
      , downSqlPath = "db/migrations/finance/0005_finance_transaction_links.down.sql"
      }
  , SqlMigration
      { migrationId = "0006_finance_transaction_notes"
      , upSqlPath = "db/migrations/finance/0006_finance_transaction_notes.up.sql"
      , downSqlPath = "db/migrations/finance/0006_finance_transaction_notes.down.sql"
      }
  ]

noteMigrations :: [SqlMigration]
noteMigrations =
  [ SqlMigration
      { migrationId = "0001_note_schema"
      , upSqlPath = "db/migrations/note/0001_note_schema.up.sql"
      , downSqlPath = "db/migrations/note/0001_note_schema.down.sql"
      }
  ]

checklistMigrations :: [SqlMigration]
checklistMigrations =
  [ SqlMigration
      { migrationId = "0001_checklist_schema"
      , upSqlPath = "db/migrations/checklist/0001_checklist_schema.up.sql"
      , downSqlPath = "db/migrations/checklist/0001_checklist_schema.down.sql"
      }
  ]

applyAllUp :: FilePath -> String -> [SqlMigration] -> ExceptT String IO MigrationStats
applyAllUp _ _ [] = pure $ MigrationStats {statsApplied = 0, statsSkipped = 0}
applyAllUp basePath connectionUrl (migration:rest) = do
  applied <- migrationAlreadyApplied connectionUrl (migrationId migration)
  if applied
    then do
      lift $ putStrLn ("[migrations] skipping already applied migration id=" ++ migrationId migration)
      stats <- applyAllUp basePath connectionUrl rest
      pure $ stats {statsSkipped = statsSkipped stats + 1}
    else do
      lift $ putStrLn ("[migrations] applying migration id=" ++ migrationId migration ++ " file=" ++ upSqlPath migration)
      runSqlFile connectionUrl (basePath ++ "/" ++ upSqlPath migration)
      markMigrationApplied connectionUrl (migrationId migration)
      lift $ putStrLn ("[migrations] applied migration id=" ++ migrationId migration)
      stats <- applyAllUp basePath connectionUrl rest
      pure $ stats {statsApplied = statsApplied stats + 1}

applyAllDown :: FilePath -> String -> [SqlMigration] -> ExceptT String IO MigrationStats
applyAllDown _ _ [] = pure $ MigrationStats {statsApplied = 0, statsSkipped = 0}
applyAllDown basePath connectionUrl (migration:rest) = do
  applied <- migrationAlreadyApplied connectionUrl (migrationId migration)
  if not applied
    then do lift $ putStrLn ("[migrations] skipping not-applied migration id=" ++ migrationId migration)
            stats <- applyAllDown basePath connectionUrl rest
            pure $ stats {statsSkipped = statsSkipped stats + 1}
    else do lift $ putStrLn ("[migrations] rolling back migration id=" ++ migrationId migration ++ " file=" ++ downSqlPath migration)
            runSqlFile connectionUrl (basePath ++ "/" ++ downSqlPath migration)
            unmarkMigrationApplied connectionUrl (migrationId migration)
            lift $ putStrLn ("[migrations] rolled back migration id=" ++ migrationId migration)
            stats <- applyAllDown basePath connectionUrl rest
            pure $ stats {statsApplied = statsApplied stats + 1}

ensureMigrationsTable :: String -> ExceptT String IO ()
ensureMigrationsTable connectionUrl =
  runSqlCommand connectionUrl "CREATE TABLE IF NOT EXISTS schema_migrations (migration_id TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW())"

migrationAlreadyApplied :: String -> String -> ExceptT String IO Bool
migrationAlreadyApplied connectionUrl name = do
  scalarResult <- runScalarQuery connectionUrl ("SELECT EXISTS(SELECT 1 FROM schema_migrations WHERE migration_id = '" ++ sqlEscape name ++ "')")
  if "t" `isInfixOf` scalarResult
    then pure True
    else if "f" `isInfixOf` scalarResult
      then pure False
      else throwE $ "Unexpected EXISTS result: " <> scalarResult

markMigrationApplied :: String -> String -> ExceptT String IO ()
markMigrationApplied connectionUrl name =
  runSqlCommand connectionUrl ("INSERT INTO schema_migrations (migration_id) VALUES ('" ++ sqlEscape name ++ "')")

unmarkMigrationApplied :: String -> String -> ExceptT String IO ()
unmarkMigrationApplied connectionUrl name =
  runSqlCommand connectionUrl ("DELETE FROM schema_migrations WHERE migration_id = '" ++ sqlEscape name ++ "'")

runSqlFile :: String -> FilePath -> ExceptT String IO ()
runSqlFile connectionUrl filePath = do
  (exitCode, _stdout, stderr) <- lift $ readProcessWithExitCode "psql" ["--dbname", connectionUrl, "-v", "ON_ERROR_STOP=1", "-f", filePath] ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwE stderr

runSqlCommand :: String -> String -> ExceptT String IO ()
runSqlCommand connectionUrl command = do
  (exitCode, _stdout, stderr) <- lift $ readProcessWithExitCode "psql" ["--dbname", connectionUrl, "-v", "ON_ERROR_STOP=1", "-c", command] ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwE stderr

runScalarQuery :: String -> String -> ExceptT String IO String
runScalarQuery connectionUrl querySql = do
  (exitCode, stdout, stderr) <- lift $ readProcessWithExitCode "psql" ["--dbname", connectionUrl, "-tA", "-c", querySql] ""
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure _ -> throwE stderr

sqlEscape :: String -> String
sqlEscape = concatMap escapeChar
  where
    escapeChar '\'' = "''"
    escapeChar c = [c]
