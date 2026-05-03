module Lib.Startup.Migrations
  ( runStartupMigrations
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, catchE, handleE)
import Data.List (intercalate)
import Lib.Config (DatabaseConfig, renderPostgresConnectionString)
import PostgresMigrations
  ( MigrationDirection(..)
  , runAuthMigrationsAtPath
  , runCalendarMigrationsAtPath
  , runChecklistMigrationsAtPath
  , runFinanceMigrationsAtPath
  , runNoteMigrationsAtPath
  , runSessionMigrationsAtPath
  , runTripSharingMigrationsAtPath
  )

runStartupMigrations :: DatabaseConfig -> ExceptT String IO ()
runStartupMigrations dbCfg = do
  let selectedDomains = ["auth", "session", "calendar", "trip-sharing", "finance", "note", "checklist"]
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
    runDomains _ [] rendered count =
      lift $ putStrLn
        ( "[startup][migrations] completed"
            ++ " direction=MigrateUp"
            ++ " domains="
            ++ show count
            ++ " selected="
            ++ rendered
        )
    runDomains connectionString (domain:rest) rendered count = do
      lift $ putStrLn ("[startup][migrations] domain=" ++ domain ++ " phase=start direction=MigrateUp")
      catchE (runDomainMigration "." connectionString domain)
             (\err -> do
                 lift $ putStrLn
                     ( "[startup][migrations] failed"
                         ++ " direction=MigrateUp"
                         ++ " domain="
                         ++ domain
                         ++ " selected="
                         ++ rendered
                         ++ " reason="
                         ++ err
                     )
                 throwE err)
      lift $ putStrLn ("[startup][migrations] domain=" ++ domain ++ " phase=done direction=MigrateUp")
      runDomains connectionString rest rendered count

runDomainMigration :: FilePath -> String -> String -> ExceptT String IO ()
runDomainMigration basePath connectionString domain = do
  handleE (\err -> throwE $ "Startup migrations failed for domain=" ++ domain ++ ": " ++ err)
          (case domain of
            "auth" -> runAuthMigrationsAtPath basePath connectionString MigrateUp
            "session" -> runSessionMigrationsAtPath basePath connectionString MigrateUp
            "calendar" -> runCalendarMigrationsAtPath basePath connectionString MigrateUp
            "trip-sharing" -> runTripSharingMigrationsAtPath basePath connectionString MigrateUp
            "finance" -> runFinanceMigrationsAtPath basePath connectionString MigrateUp
            "note" -> runNoteMigrationsAtPath basePath connectionString MigrateUp
            "checklist" -> runChecklistMigrationsAtPath basePath connectionString MigrateUp
            _ -> throwE $ "Unsupported migration domain: " <> domain)
  pure ()
