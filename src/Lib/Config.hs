{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.Config
  ( AppConfig(..)
  , DatabaseConfig(..)
  , Backend(..)
  , loadAppConfigFromFile
  , startupMigrationDomainsForBackend
  , renderPostgresConnectionString
  , renderDatabaseTarget
  ) where

import Data.Aeson (FromJSON(parseJSON), eitherDecodeFileStrict', (.:), (.:?), withObject)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE, catchE)
import Session (SessionConfig(..), defaultSessionConfig)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

data SessionConfigFile = SessionConfigFile
  { sessionCookieNameFile :: !(Maybe String)
  , sessionAbsoluteTtlSecondsFile :: !(Maybe Int)
  , sessionIdleTtlSecondsFile :: !(Maybe Int)
  } deriving (Generic)

instance FromJSON SessionConfigFile where
  parseJSON = withObject "SessionConfigFile" $ \v -> SessionConfigFile
    <$> v .:? "cookieName"
    <*> v .:? "absoluteTtlSeconds"
    <*> v .:? "idleTtlSeconds"

newtype AuthConfigFile = AuthConfigFile
  { bootstrapAdminUsernameFile :: String
  } deriving (Generic)

instance FromJSON AuthConfigFile where
  parseJSON = withObject "AuthConfigFile" $ \v -> AuthConfigFile
    <$> v .: "bootstrapAdminUsername"

data AppConfigFile = AppConfigFile
  { appSession :: !SessionConfigFile
  , appAuth :: !AuthConfigFile
  , appBackendFile :: !(Maybe String)
  , appDatabase :: !(Maybe DatabaseConfigFile)
  } deriving (Generic)

instance FromJSON AppConfigFile where
  parseJSON = withObject "AppConfigFile" $ \v -> AppConfigFile
    <$> v .: "session"
    <*> v .: "auth"
    <*> v .:? "backend"
    <*> v .:? "database"

data DatabaseConfigFile = DatabaseConfigFile
  { databaseHostFile :: !String
  , databasePortFile :: !Int
  , databaseNameFile :: !String
  , databaseUserFile :: !String
  , databasePasswordFile :: !String
  } deriving (Generic)

instance FromJSON DatabaseConfigFile where
  parseJSON = withObject "DatabaseConfigFile" $ \v -> DatabaseConfigFile
    <$> v .: "host"
    <*> v .: "port"
    <*> v .: "name"
    <*> v .: "user"
    <*> v .: "password"

data Backend = Filesystem | Postgres
  deriving (Show, Eq)

data AppConfig = AppConfig
  { sessionConfig :: !SessionConfig
  , bootstrapAdminUsername :: !String
  , backend :: !Backend
  , databaseConfig :: !(Maybe DatabaseConfig)
  }

data DatabaseConfig = DatabaseConfig
  { databaseHost :: !String
  , databasePort :: !Int
  , databaseName :: !String
  , databaseUser :: !String
  , databasePassword :: !String
  }

loadAppConfigFromFile :: ExceptT String IO AppConfig
loadAppConfigFromFile = do
  mSecret <- lift $ lookupEnv "FOUCL_SESSION_SECRET"
  mConfigPath <- lift $ lookupEnv "FOUCL_CONFIG_FILE"
  mCookieSecureRaw <- lift $ lookupEnv "FOUCL_SESSION_COOKIE_SECURE"
  let configPath = fromMaybe "config/app-config.json" mConfigPath
  case mSecret of
    Nothing -> throwE "Missing required environment variable FOUCL_SESSION_SECRET"
    Just secret | null secret -> throwE "Environment variable FOUCL_SESSION_SECRET cannot be empty"
    Just secret -> do
      exists <- lift $ doesFileExist configPath
      if not exists
        then throwE ("Missing configuration file " ++ configPath)
        else do
          fileConfig <- catchE (ExceptT (eitherDecodeFileStrict' configPath :: IO (Either String AppConfigFile)))
                               (\err -> throwE $ "Unable to parse configuration file: " ++ err)
          either throwE pure $ toAppConfig secret fileConfig (parseBool =<< mCookieSecureRaw)

toAppConfig :: String -> AppConfigFile -> Maybe Bool -> Either String AppConfig
toAppConfig secret AppConfigFile {appSession = SessionConfigFile {sessionCookieNameFile, sessionAbsoluteTtlSecondsFile, sessionIdleTtlSecondsFile}, appAuth = AuthConfigFile {bootstrapAdminUsernameFile}, appBackendFile, appDatabase} mCookieSecure
  | null bootstrapAdminUsernameFile = Left "Configuration auth.bootstrapAdminUsername cannot be empty"
  | otherwise = do
      selectedBackend <-
        case appBackendFile of
          Nothing -> Right Filesystem
          Just "filesystem" -> Right Filesystem
          Just "postgres" -> Right Postgres
          Just _ -> Left "Configuration backend must be one of: filesystem, postgres"
      parsedDatabaseConfig <- traverse validateDatabaseConfig appDatabase
      pure AppConfig
        { sessionConfig =
            defaultSessionConfig
              { sessionSecret = secret
              , sessionCookieName = fromMaybe (sessionCookieName defaultSessionConfig) sessionCookieNameFile
              , sessionAbsoluteTtlSeconds = fromIntegral (fromMaybe (round (sessionAbsoluteTtlSeconds defaultSessionConfig)) sessionAbsoluteTtlSecondsFile)
              , sessionIdleTtlSeconds = fromIntegral (fromMaybe (round (sessionIdleTtlSeconds defaultSessionConfig)) sessionIdleTtlSecondsFile)
              , sessionCookieSecure = fromMaybe (sessionCookieSecure defaultSessionConfig) mCookieSecure
              }
        , bootstrapAdminUsername = bootstrapAdminUsernameFile
        , backend = selectedBackend
        , databaseConfig = parsedDatabaseConfig
        }

parseBool :: String -> Maybe Bool
parseBool raw =
  case map toLower raw of
    "true" -> Just True
    "1" -> Just True
    "false" -> Just False
    "0" -> Just False
    _ -> Nothing

startupMigrationDomainsForBackend :: Backend -> [String]
startupMigrationDomainsForBackend Filesystem = []
startupMigrationDomainsForBackend Postgres =
  ["auth", "session", "calendar", "trip-sharing", "finance", "note", "checklist"]

validateDatabaseConfig :: DatabaseConfigFile -> Either String DatabaseConfig
validateDatabaseConfig DatabaseConfigFile {databaseHostFile, databasePortFile, databaseNameFile, databaseUserFile, databasePasswordFile}
  | null databaseHostFile = Left "Configuration database.host cannot be empty"
  | databasePortFile <= 0 = Left "Configuration database.port must be a positive integer"
  | null databaseNameFile = Left "Configuration database.name cannot be empty"
  | null databaseUserFile = Left "Configuration database.user cannot be empty"
  | null databasePasswordFile = Left "Configuration database.password cannot be empty"
  | otherwise =
      Right DatabaseConfig
        { databaseHost = databaseHostFile
        , databasePort = databasePortFile
        , databaseName = databaseNameFile
        , databaseUser = databaseUserFile
        , databasePassword = databasePasswordFile
        }

renderPostgresConnectionString :: DatabaseConfig -> String
renderPostgresConnectionString cfg =
  unwords
    [ "host=" ++ pgQuote (databaseHost cfg)
    , "port=" ++ show (databasePort cfg)
    , "dbname=" ++ pgQuote (databaseName cfg)
    , "user=" ++ pgQuote (databaseUser cfg)
    , "password=" ++ pgQuote (databasePassword cfg)
    ]

renderDatabaseTarget :: DatabaseConfig -> String
renderDatabaseTarget cfg =
  "host=" ++ databaseHost cfg
    ++ " port=" ++ show (databasePort cfg)
    ++ " dbname=" ++ databaseName cfg
    ++ " user=" ++ databaseUser cfg

pgQuote :: String -> String
pgQuote raw = "'" ++ concatMap escape raw ++ "'"
  where
    escape '\'' = "\\'"
    escape '\\' = "\\\\"
    escape c = [c]
