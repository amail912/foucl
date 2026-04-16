module PostgresMigrations
  ( MigrationDirection(..)
  , runAuthMigrations
  , runAuthMigrationsAtPath
  , runSessionMigrations
  , runSessionMigrationsAtPath
  , psqlAvailable
  ) where

import Control.Monad (unless, when)
import Data.List (isInfixOf)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

data MigrationDirection
  = MigrateUp
  | MigrateDown
  deriving (Eq, Show)

data SqlMigration = SqlMigration
  { migrationId :: String
  , upSqlPath :: FilePath
  , downSqlPath :: FilePath
  }

data MigrationStats = MigrationStats
  { statsApplied :: Int
  , statsSkipped :: Int
  }

runAuthMigrations :: String -> MigrationDirection -> IO (Either String ())
runAuthMigrations = runAuthMigrationsAtPath "."

runAuthMigrationsAtPath :: FilePath -> String -> MigrationDirection -> IO (Either String ())
runAuthMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "auth" basePath connectionUrl direction authMigrations

runSessionMigrations :: String -> MigrationDirection -> IO (Either String ())
runSessionMigrations = runSessionMigrationsAtPath "."

runSessionMigrationsAtPath :: FilePath -> String -> MigrationDirection -> IO (Either String ())
runSessionMigrationsAtPath basePath connectionUrl direction =
  runMigrationsAtPath "session" basePath connectionUrl direction sessionMigrations

runMigrationsAtPath :: String -> FilePath -> String -> MigrationDirection -> [SqlMigration] -> IO (Either String ())
runMigrationsAtPath domain basePath connectionUrl direction migrations = do
  startedAt <- getCurrentTime
  putStrLn ("[migrations] starting " ++ domain ++ " migrations direction=" ++ show direction)
  available <- psqlAvailable
  if not available
    then pure (Left "psql binary not found in PATH")
    else do
      ensureResult <- ensureMigrationsTable connectionUrl
      case ensureResult of
        Left err -> pure (Left err)
        Right () -> do
          migrationResult <-
            case direction of
              MigrateUp -> applyAllUp basePath connectionUrl migrations
              MigrateDown -> applyAllDown basePath connectionUrl (reverse migrations)
          endedAt <- getCurrentTime
          let elapsed = diffUTCTime endedAt startedAt
          case migrationResult of
            Left err -> do
              putStrLn ("[migrations] " ++ domain ++ " migrations failed direction=" ++ show direction ++ " elapsed=" ++ show elapsed)
              pure (Left err)
            Right stats -> do
              putStrLn ("[migrations] " ++ domain ++ " migrations completed direction=" ++ show direction ++ " applied=" ++ show (statsApplied stats) ++ " skipped=" ++ show (statsSkipped stats) ++ " elapsed=" ++ show elapsed)
              pure (Right ())

psqlAvailable :: IO Bool
psqlAvailable = do
  mPath <- findExecutable "psql"
  pure (maybe False (const True) mPath)

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

applyAllUp :: FilePath -> String -> [SqlMigration] -> IO (Either String MigrationStats)
applyAllUp _ _ [] = pure (Right MigrationStats {statsApplied = 0, statsSkipped = 0})
applyAllUp basePath connectionUrl (migration:rest) = do
  applied <- migrationAlreadyApplied connectionUrl (migrationId migration)
  case applied of
    Left err -> pure (Left err)
    Right True -> do
      putStrLn ("[migrations] skipping already applied migration id=" ++ migrationId migration)
      restResult <- applyAllUp basePath connectionUrl rest
      pure $ fmap (\stats -> stats {statsSkipped = statsSkipped stats + 1}) restResult
    Right False -> do
      putStrLn ("[migrations] applying migration id=" ++ migrationId migration ++ " file=" ++ upSqlPath migration)
      execResult <- runSqlFile connectionUrl (basePath ++ "/" ++ upSqlPath migration)
      case execResult of
        Left err -> pure (Left err)
        Right () -> do
          markResult <- markMigrationApplied connectionUrl (migrationId migration)
          case markResult of
            Left err -> pure (Left err)
            Right () -> do
              putStrLn ("[migrations] applied migration id=" ++ migrationId migration)
              restResult <- applyAllUp basePath connectionUrl rest
              pure $ fmap (\stats -> stats {statsApplied = statsApplied stats + 1}) restResult

applyAllDown :: FilePath -> String -> [SqlMigration] -> IO (Either String MigrationStats)
applyAllDown _ _ [] = pure (Right MigrationStats {statsApplied = 0, statsSkipped = 0})
applyAllDown basePath connectionUrl (migration:rest) = do
  applied <- migrationAlreadyApplied connectionUrl (migrationId migration)
  case applied of
    Left err -> pure (Left err)
    Right False -> do
      putStrLn ("[migrations] skipping not-applied migration id=" ++ migrationId migration)
      restResult <- applyAllDown basePath connectionUrl rest
      pure $ fmap (\stats -> stats {statsSkipped = statsSkipped stats + 1}) restResult
    Right True -> do
      putStrLn ("[migrations] rolling back migration id=" ++ migrationId migration ++ " file=" ++ downSqlPath migration)
      execResult <- runSqlFile connectionUrl (basePath ++ "/" ++ downSqlPath migration)
      case execResult of
        Left err -> pure (Left err)
        Right () -> do
          unmarkResult <- unmarkMigrationApplied connectionUrl (migrationId migration)
          case unmarkResult of
            Left err -> pure (Left err)
            Right () -> do
              putStrLn ("[migrations] rolled back migration id=" ++ migrationId migration)
              restResult <- applyAllDown basePath connectionUrl rest
              pure $ fmap (\stats -> stats {statsApplied = statsApplied stats + 1}) restResult

ensureMigrationsTable :: String -> IO (Either String ())
ensureMigrationsTable connectionUrl =
  runSqlCommand connectionUrl "CREATE TABLE IF NOT EXISTS schema_migrations (migration_id TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW())"

migrationAlreadyApplied :: String -> String -> IO (Either String Bool)
migrationAlreadyApplied connectionUrl name = do
  scalarResult <- runScalarQuery connectionUrl ("SELECT EXISTS(SELECT 1 FROM schema_migrations WHERE migration_id = '" ++ sqlEscape name ++ "')")
  case scalarResult of
    Left err -> pure (Left err)
    Right value
      | "t" `isInfixOf` value -> pure (Right True)
      | "f" `isInfixOf` value -> pure (Right False)
      | otherwise -> pure (Left ("Unexpected EXISTS result: " ++ value))

markMigrationApplied :: String -> String -> IO (Either String ())
markMigrationApplied connectionUrl name =
  runSqlCommand connectionUrl ("INSERT INTO schema_migrations (migration_id) VALUES ('" ++ sqlEscape name ++ "')")

unmarkMigrationApplied :: String -> String -> IO (Either String ())
unmarkMigrationApplied connectionUrl name =
  runSqlCommand connectionUrl ("DELETE FROM schema_migrations WHERE migration_id = '" ++ sqlEscape name ++ "'")

runSqlFile :: String -> FilePath -> IO (Either String ())
runSqlFile connectionUrl filePath = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode "psql" ["--dbname", connectionUrl, "-v", "ON_ERROR_STOP=1", "-f", filePath] ""
  pure $
    case exitCode of
      ExitSuccess -> Right ()
      ExitFailure _ -> Left stderr

runSqlCommand :: String -> String -> IO (Either String ())
runSqlCommand connectionUrl command = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode "psql" ["--dbname", connectionUrl, "-v", "ON_ERROR_STOP=1", "-c", command] ""
  pure $
    case exitCode of
      ExitSuccess -> Right ()
      ExitFailure _ -> Left stderr

runScalarQuery :: String -> String -> IO (Either String String)
runScalarQuery connectionUrl querySql = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "psql" ["--dbname", connectionUrl, "-tA", "-c", querySql] ""
  pure $
    case exitCode of
      ExitSuccess -> Right stdout
      ExitFailure _ -> Left stderr

sqlEscape :: String -> String
sqlEscape = concatMap escapeChar
  where
    escapeChar '\'' = "''"
    escapeChar c = [c]
