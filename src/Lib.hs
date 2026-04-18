{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( runApp
    , AuthBackend(..)
    , parseAuthBackend
    , SessionBackend(..)
    , parseSessionBackend
    , CalendarBackend(..)
    , parseCalendarBackend
    , TripSharingBackend(..)
    , parseTripSharingBackend
    , NoteBackend(..)
    , parseNoteBackend
    , ChecklistBackend(..)
    , parseChecklistBackend
    , makeSessionStore
    , makeCalendarRepository
    , makeTripSharingRepository
    , makeNoteRepository
    , makeChecklistRepository
    , DatabaseConfig(..)
    , startupMigrationDomainsForBackends
    ) where

import Prelude hiding (log, writeFile)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), decode, encode, decode', eitherDecodeFileStrict', (.:), (.:?), (.=), withObject, object)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe, mapMaybe, catMaybes)
import Data.Int (Int64)
import Data.List (intercalate, isPrefixOf, nub, sort, sortOn)
import Control.Monad (msum, mzero, join, foldM, when, mplus)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Either (either)
import Data.ByteString.Char8 (unpack)
import Data.Char (toLower)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Control.Exception as Ex
import Happstack.Server (FilterMonad, Response, ServerPartT, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse, method, ok, internalServerError, notFound, dir, Method(GET, POST, DELETE, PUT), Conf(..), addCookie, mkCookie, CookieLife(Session, Expired), getHeaderM, unauthorized, requestEntityTooLarge, look, setResponseCode)
import qualified Happstack.Server as HServer
import Happstack.Server.Internal.Cookie (Cookie(..), SameSite(..))
import Happstack.Server.Internal.MessageWrap (bodyInput, BodyPolicy)
import Model (NoteContent, ChecklistContent, Content, Identifiable(..))
import qualified Model
import qualified AgendaModel as Agenda
import CalendarRepository (CalendarRepository(..), defaultCalendarRepository, postgresCalendarRepository, verifyPostgresCalendarStorage)
import TripSharingRepository (TripSharingRepository(..), defaultTripSharingRepository, postgresTripSharingRepository, verifyPostgresTripSharingStorage)
import CrudStorage (createItem, getAllItems, deleteItem, modifyItem)
import Crud
import NoteCrud (NoteServiceConfig(..), defaultNoteServiceConfig)
import ChecklistCrud (ChecklistServiceConfig(..), defaultChecklistServiceConfig)
import NotesChecklistRepository
  ( NotesChecklistRepository(..)
  , NoteRepository
  , ChecklistRepository
  , defaultChecklistRepository
  , defaultNoteRepository
  , postgresChecklistRepository
  , postgresNoteRepository
  , verifyPostgresChecklistStorage
  , verifyPostgresNoteStorage
  )
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, getCurrentDirectory, canonicalizePath, getTemporaryDirectory)
import System.FilePath ((</>), pathSeparator, takeBaseName, takeExtension)
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 (writeFile)
import Filesystem.Path.CurrentOS    (commonPrefix, encodeString, decodeString, collapse, append)
import Auth (AuthRequest(..), AuthRequestError(..), AuthError(..), AuthenticatedProfile(..), AuthRepository, defaultAuthRepository, createUserWithBootstrapAdmin, loadAuthenticatedProfile, signinUser, userExists, isApprovedAdmin, listPendingUsers, listApprovedUsers, approveUser, deletePendingUser, deleteApprovedUser)
import qualified AuthRepository
import Session (SessionConfig(..), SessionPrincipal(..), SessionStore(..), SessionState(..), SessionHandle(..), UserStateBinding(..), SessionRepository(..), defaultSessionConfig, mkFileSessionStore, mkPostgresSessionRepository, mkSessionStore, signSessionId, verifyPostgresSessionStorage, verifyAndExtractSessionId)
import Repository (RepositoryError(..))
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError(..), close, connectPostgreSQL, execute, query, query_)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import PostgresMigrations
  ( MigrationDirection(..)
  , runAuthMigrationsAtPath
  , runSessionMigrationsAtPath
  , runCalendarMigrationsAtPath
  , runTripSharingMigrationsAtPath
  , runNoteMigrationsAtPath
  , runChecklistMigrationsAtPath
  )

type AppM a = ExceptT String (ServerPartT IO) a


data SessionConfigFile = SessionConfigFile
  { sessionCookieNameFile :: !(Maybe String)
  , sessionAbsoluteTtlSecondsFile :: !(Maybe Int)
  , sessionIdleTtlSecondsFile :: !(Maybe Int)
  , sessionBackendFile :: !(Maybe String)
  } deriving (Generic)

instance FromJSON SessionConfigFile where
  parseJSON = withObject "SessionConfigFile" $ \v -> SessionConfigFile
    <$> v .:? "cookieName"
    <*> v .:? "absoluteTtlSeconds"
    <*> v .:? "idleTtlSeconds"
    <*> v .:? "sessionBackend"

data AuthConfigFile = AuthConfigFile
  { bootstrapAdminUsernameFile :: String
  , authBackendFile :: !(Maybe String)
  } deriving (Generic)

instance FromJSON AuthConfigFile where
  parseJSON = withObject "AuthConfigFile" $ \v -> AuthConfigFile
    <$> v .: "bootstrapAdminUsername"
    <*> v .:? "authBackend"

data AppConfigFile = AppConfigFile
  { appSession :: SessionConfigFile
  , appAuth :: AuthConfigFile
  , appCalendarBackendFile :: !(Maybe String)
  , appTripSharingBackendFile :: !(Maybe String)
  , appNoteBackendFile :: !(Maybe String)
  , appChecklistBackendFile :: !(Maybe String)
  , appDatabase :: !(Maybe DatabaseConfigFile)
  } deriving (Generic)

instance FromJSON AppConfigFile where
  parseJSON = withObject "AppConfigFile" $ \v -> AppConfigFile
    <$> v .: "session"
    <*> v .: "auth"
    <*> v .:? "calendarBackend"
    <*> v .:? "tripSharingBackend"
    <*> v .:? "noteBackend"
    <*> v .:? "checklistBackend"
    <*> v .:? "database"

data DatabaseConfigFile = DatabaseConfigFile
  { databaseHostFile :: String
  , databasePortFile :: Int
  , databaseNameFile :: String
  , databaseUserFile :: String
  , databasePasswordFile :: String
  } deriving (Generic)

instance FromJSON DatabaseConfigFile where
  parseJSON = withObject "DatabaseConfigFile" $ \v -> DatabaseConfigFile
    <$> v .: "host"
    <*> v .: "port"
    <*> v .: "name"
    <*> v .: "user"
    <*> v .: "password"

data AppConfig = AppConfig
  { sessionConfig :: SessionConfig
  , sessionBackend :: SessionBackend
  , bootstrapAdminUsername :: String
  , authBackend :: AuthBackend
  , calendarBackend :: CalendarBackend
  , tripSharingBackend :: TripSharingBackend
  , noteBackend :: NoteBackend
  , checklistBackend :: ChecklistBackend
  , databaseConfig :: !(Maybe DatabaseConfig)
  }

data DatabaseConfig = DatabaseConfig
  { databaseHost :: String
  , databasePort :: Int
  , databaseName :: String
  , databaseUser :: String
  , databasePassword :: String
  }

data AuthBackend
  = AuthBackendFilesystem
  | AuthBackendPostgres
  deriving (Eq, Show)

data SessionBackend
  = SessionBackendFilesystem
  | SessionBackendPostgres
  deriving (Eq, Show)

data CalendarBackend
  = CalendarBackendFilesystem
  | CalendarBackendPostgres
  deriving (Eq, Show)

data TripSharingBackend
  = TripSharingBackendFilesystem
  | TripSharingBackendPostgres
  deriving (Eq, Show)

data NoteBackend
  = NoteBackendFilesystem
  | NoteBackendPostgres
  deriving (Eq, Show)

data ChecklistBackend
  = ChecklistBackendFilesystem
  | ChecklistBackendPostgres
  deriving (Eq, Show)

newtype AppContext = AppContext
  { sessionPrincipal :: SessionPrincipal
  }

newtype TripPlace = TripPlace
  { tripPlaceName :: String
  }

newtype TripSharingUser = TripSharingUser
  { tripSharingUsername :: String
  }

newtype PendingSignupApproval = PendingSignupApproval
  { pendingSignupApprovalUsername :: String
  }

data PeriodTripsUser = PeriodTripsUser
  { periodTripsUsername :: String
  , periodTripsItems :: [Agenda.CalendarItem]
  }

data StoredTripItem = StoredTripItem
  { storedTripStart :: LocalTime
  , storedTripCalendarItem :: Agenda.CalendarItem
  }

instance ToJSON TripPlace where
  toJSON (TripPlace placeName) = object ["name" .= placeName]

instance ToJSON TripSharingUser where
  toJSON (TripSharingUser username) = object ["username" .= username]

instance ToJSON PendingSignupApproval where
  toJSON (PendingSignupApproval username) = object ["username" .= username]

instance ToJSON PeriodTripsUser where
  toJSON (PeriodTripsUser username trips) =
    object
      [ "username" .= username
      , "trips" .= trips
      ]

instance FromJSON TripSharingUser where
  parseJSON = withObject "TripSharingUser" $ \value -> TripSharingUser
    <$> value .: "username"

instance FromJSON PendingSignupApproval where
  parseJSON = withObject "PendingSignupApproval" $ \value -> PendingSignupApproval
    <$> value .: "username"

tripPlacesCatalog :: [TripPlace]
tripPlacesCatalog =
  [ TripPlace "Paris"
  , TripPlace "Le Mesnil"
  , TripPlace "St Clair"
  ]

data TripWriteValidation
  = TripWriteValid
  | TripWriteBadRequest String
  | TripWriteNotFound
  | TripWriteTechnicalFailure

tripPlaceNames :: [String]
tripPlaceNames = map tripPlaceName tripPlacesCatalog

validateTripWrite :: CalendarRepository -> String -> Maybe String -> Agenda.CalendarItemContent -> IO TripWriteValidation
validateTripWrite calendarRepo principalUserId mCurrentItemId content =
  case content of
    Agenda.TripCalendarItemContent tripContent -> do
      existingItemsResult <- runExceptT (repoListCalendarItemsForUser calendarRepo principalUserId)
      pure $
        case existingItemsResult of
          Left _ -> TripWriteTechnicalFailure
          Right existingItems -> validateTripContent tripPlaceNames existingItems mCurrentItemId tripContent
    _ -> pure TripWriteValid

validateTripContent :: [String] -> [Agenda.CalendarItem] -> Maybe String -> Agenda.TripItemContent -> TripWriteValidation
validateTripContent validPlaceNames existingItems mCurrentItemId tripContent =
  case validateTripWritePreconditions existingItems mCurrentItemId of
    Just result -> result
    Nothing ->
      case tripInterval tripContent of
        Nothing -> TripWriteBadRequest "windowStart and windowEnd must be valid ISO date-time strings"
        Just interval
          | Agenda.departurePlaceId tripContent `notElem` validPlaceNames ->
              TripWriteBadRequest "departurePlaceId must reference an existing trip place"
          | Agenda.arrivalPlaceId tripContent `notElem` validPlaceNames ->
              TripWriteBadRequest "arrivalPlaceId must reference an existing trip place"
          | Agenda.departurePlaceId tripContent == Agenda.arrivalPlaceId tripContent ->
              TripWriteBadRequest "departurePlaceId and arrivalPlaceId must be different"
          | not (tripIntervalHasPositiveDuration interval) ->
              TripWriteBadRequest "windowEnd must be strictly after windowStart"
          | otherwise ->
              case storedTripIntervals existingItems mCurrentItemId of
                Left () -> TripWriteTechnicalFailure
                Right intervals
                  | any (tripIntervalsOverlap interval) intervals ->
                      TripWriteBadRequest "trip time window overlaps another trip"
                  | otherwise -> TripWriteValid

validateTripWritePreconditions :: [Agenda.CalendarItem] -> Maybe String -> Maybe TripWriteValidation
validateTripWritePreconditions existingItems mCurrentItemId =
  case mCurrentItemId of
    Just currentItemId
      | currentItemId `notElem` mapMaybe calendarItemId existingItems -> Just TripWriteNotFound
    _ -> Nothing

calendarItemId :: Agenda.CalendarItem -> Maybe String
calendarItemId item =
  case item of
    Agenda.ServerCalendarItem { Agenda.itemId } -> Just itemId
    Agenda.NewCalendarItem {} -> Nothing

tripInterval :: Agenda.TripItemContent -> Maybe (LocalTime, LocalTime)
tripInterval tripContent = do
  start <- parseTripLocalTime (Agenda.tripWindowStart tripContent)
  end <- parseTripLocalTime (Agenda.tripWindowEnd tripContent)
  pure (start, end)

parseTripLocalTime :: String -> Maybe LocalTime
parseTripLocalTime raw =
  iso8601ParseM raw `mplus` parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" raw

tripIntervalHasPositiveDuration :: (LocalTime, LocalTime) -> Bool
tripIntervalHasPositiveDuration (start, end) = end > start

-- Touching boundaries are allowed; only real interval intersection is rejected.
tripIntervalsOverlap :: (LocalTime, LocalTime) -> (LocalTime, LocalTime) -> Bool
tripIntervalsOverlap (startA, endA) (startB, endB) = startA < endB && startB < endA

storedTripIntervals :: [Agenda.CalendarItem] -> Maybe String -> Either () [(LocalTime, LocalTime)]
storedTripIntervals existingItems mCurrentItemId =
  mapM storedTripInterval (filter isOtherStoredTrip existingItems)
  where
    isOtherStoredTrip item =
      case item of
        Agenda.ServerCalendarItem { Agenda.content = Agenda.TripCalendarItemContent {}, Agenda.itemId } ->
          Just itemId /= mCurrentItemId
        _ -> False

    storedTripInterval item =
      case item of
        Agenda.ServerCalendarItem { Agenda.content = Agenda.TripCalendarItemContent tripContent } ->
          maybe (Left ()) Right (tripInterval tripContent)
        _ -> Left ()

parsePeriodTripBounds :: Maybe String -> Maybe String -> Either String (LocalTime, LocalTime)
parsePeriodTripBounds Nothing _ = Left "start is required"
parsePeriodTripBounds _ Nothing = Left "end is required"
parsePeriodTripBounds (Just rawStart) (Just rawEnd) =
  case (parseTripLocalTime rawStart, parseTripLocalTime rawEnd) of
    (Nothing, _) -> Left "start must be a valid ISO date-time string"
    (_, Nothing) -> Left "end must be a valid ISO date-time string"
    (Just start, Just end)
      | end <= start -> Left "end must be strictly after start"
      | otherwise -> Right (start, end)

resolveVisiblePeriodTripUsers :: TripSharingRepository -> String -> IO (Either () [String])
resolveVisiblePeriodTripUsers tripSharingRepo principalUserId = do
  subscribedUsersResult <- runExceptT (repoListSubscribedUsers tripSharingRepo principalUserId)
  case subscribedUsersResult of
    Left _ -> pure (Left ())
    Right subscribedUsers -> do
      visibilityResults <- mapM isVisibleToPrincipal subscribedUsers
      pure $ case sequence visibilityResults of
        Left _ -> Left ()
        Right visibleUsers -> Right [username | (username, True) <- visibleUsers]
  where
    isVisibleToPrincipal username = do
      sharedUsersResult <- runExceptT (repoListSharedUsers tripSharingRepo username)
      pure $ case sharedUsersResult of
        Left _ -> Left ()
        Right sharedUsers -> Right (username, principalUserId `elem` sharedUsers)

loadPeriodTripsForUsers :: CalendarRepository -> [String] -> LocalTime -> LocalTime -> IO (Either () [PeriodTripsUser])
loadPeriodTripsForUsers calendarRepo usernames periodStart periodEnd = do
  groups <- mapM buildUserGroup usernames
  pure $ fmap catMaybes (sequence groups)
  where
    buildUserGroup username = do
      itemsResult <- runExceptT (repoListCalendarItemsForUser calendarRepo username)
      pure $
        case itemsResult of
          Left _ -> Left ()
          Right items ->
            case selectPeriodTrips periodStart periodEnd items of
              Left () -> Left ()
              Right [] -> Right Nothing
              Right trips -> Right (Just (PeriodTripsUser username trips))

selectPeriodTrips :: LocalTime -> LocalTime -> [Agenda.CalendarItem] -> Either () [Agenda.CalendarItem]
selectPeriodTrips periodStart periodEnd items = do
  storedTrips <- storedTripItems items
  let orderedTrips = sortOn storedTripStart storedTrips
      seedTrip = case filter (\trip -> storedTripStart trip < periodStart) orderedTrips of
        [] -> Nothing
        earlierTrips -> Just (last earlierTrips)
      periodTrips =
        [ storedTripCalendarItem trip
        | trip <- orderedTrips
        , storedTripStart trip >= periodStart
        , storedTripStart trip < periodEnd
        ]
  pure $
    case seedTrip of
      Nothing -> periodTrips
      Just trip -> storedTripCalendarItem trip : periodTrips

storedTripItems :: [Agenda.CalendarItem] -> Either () [StoredTripItem]
storedTripItems items = catMaybes <$> mapM toStoredTripItem items
  where
    toStoredTripItem item =
      case item of
        Agenda.ServerCalendarItem { Agenda.content = Agenda.TripCalendarItemContent tripContent } ->
          case parseTripLocalTime (Agenda.tripWindowStart tripContent) of
            Nothing -> Left ()
            Just tripStart -> Right (Just (StoredTripItem tripStart item))
        _ -> Right Nothing

badRequest :: FilterMonad Response m => String -> m Response
badRequest = HServer.badRequest . jsonMessage

jsonResponse :: ToJSON a => a -> Response
jsonResponse = toResponse . encode

jsonMessage :: String -> Response
jsonMessage msg = jsonResponse $ object ["message" .= msg]

emptyResponse :: Response
emptyResponse = jsonResponse $ object []

authInternalError :: Response
authInternalError = jsonMessage "Unable to process authentication"

class ToServerResponse e where
  toServerResponse :: Monad m => e -> ServerPartT m Response

instance ToServerResponse AuthError where
  toServerResponse (BadRequest br) = badRequest errorStr
    where errorStr = case br of
                     EmptyUsername -> "Username cannot be empty"
                     UsernameDoesNotRespectPattern -> "Username has forbidden characters"
                     EmptyPassword -> "Password cannot be empty"
                     PasswordTooShort -> "Password is too short"
                     UsernameTooShort -> "Username is too short"
                     UsernameTooLong -> "Username is too long"
  toServerResponse UserAlreadyExists = badRequest "Unable to create user"
  toServerResponse InvalidCredentials = unauthorized $ jsonMessage "Invalid credentials"
  toServerResponse AccountPendingApproval = HServer.forbidden $ jsonMessage "Account pending approval"
  toServerResponse ResourceNotFound = notFound $ jsonMessage "Not found"
  toServerResponse (ResourceConflict message) = setResponseCode 409 >> pure (jsonMessage message)
  toServerResponse (TechnicalError _) = internalServerError $ jsonMessage "Unable to process authentication"

loadAppConfigFromFile :: IO (Either String AppConfig)
loadAppConfigFromFile = do
  mSecret <- lookupEnv "FOUCL_SESSION_SECRET"
  mConfigPath <- lookupEnv "FOUCL_CONFIG_FILE"
  mCookieSecureRaw <- lookupEnv "FOUCL_SESSION_COOKIE_SECURE"
  let configPath = fromMaybe "config/app-config.json" mConfigPath
  case mSecret of
    Nothing -> pure $ Left "Missing required environment variable FOUCL_SESSION_SECRET"
    Just secret | null secret -> pure $ Left "Environment variable FOUCL_SESSION_SECRET cannot be empty"
    Just secret -> do
      exists <- doesFileExist configPath
      if not exists
        then pure $ Left ("Missing configuration file " ++ configPath)
        else do
          decoded <- eitherDecodeFileStrict' configPath :: IO (Either String AppConfigFile)
          case decoded of
            Left err -> pure $ Left ("Unable to parse configuration file: " ++ err)
            Right fileConfig ->
              case toAppConfig secret fileConfig (parseBool =<< mCookieSecureRaw) of
                Left err -> pure $ Left err
                Right appConfig -> pure $ Right appConfig

toAppConfig :: String -> AppConfigFile -> Maybe Bool -> Either String AppConfig
toAppConfig secret AppConfigFile {appSession = SessionConfigFile {sessionCookieNameFile, sessionAbsoluteTtlSecondsFile, sessionIdleTtlSecondsFile, sessionBackendFile}, appAuth = AuthConfigFile {bootstrapAdminUsernameFile, authBackendFile}, appCalendarBackendFile, appTripSharingBackendFile, appNoteBackendFile, appChecklistBackendFile, appDatabase} mCookieSecure
  | null bootstrapAdminUsernameFile = Left "Configuration auth.bootstrapAdminUsername cannot be empty"
  | otherwise =
      case parseSessionBackend sessionBackendFile of
        Left err -> Left err
        Right selectedSessionBackend ->
          case parseAuthBackend authBackendFile of
            Left err -> Left err
            Right selectedAuthBackend ->
              case parseCalendarBackend appCalendarBackendFile of
                Left err -> Left err
                Right selectedCalendarBackend ->
                  case parseTripSharingBackend appTripSharingBackendFile of
                    Left err -> Left err
                    Right selectedTripSharingBackend ->
                      case parseNoteBackend appNoteBackendFile of
                        Left err -> Left err
                        Right selectedNoteBackend ->
                          case parseChecklistBackend appChecklistBackendFile of
                            Left err -> Left err
                            Right selectedChecklistBackend ->
                              case traverse validateDatabaseConfig appDatabase of
                                Left err -> Left err
                                Right parsedDatabaseConfig ->
                                  Right AppConfig
                                    { sessionConfig =
                                        defaultSessionConfig
                                          { sessionSecret = secret
                                          , sessionCookieName = fromMaybe (sessionCookieName defaultSessionConfig) sessionCookieNameFile
                                          , sessionAbsoluteTtlSeconds = fromIntegral (fromMaybe (round (sessionAbsoluteTtlSeconds defaultSessionConfig)) sessionAbsoluteTtlSecondsFile)
                                          , sessionIdleTtlSeconds = fromIntegral (fromMaybe (round (sessionIdleTtlSeconds defaultSessionConfig)) sessionIdleTtlSecondsFile)
                                          , sessionCookieSecure = fromMaybe (sessionCookieSecure defaultSessionConfig) mCookieSecure
                                          }
                                    , sessionBackend = selectedSessionBackend
                                    , bootstrapAdminUsername = bootstrapAdminUsernameFile
                                    , authBackend = selectedAuthBackend
                                    , calendarBackend = selectedCalendarBackend
                                    , tripSharingBackend = selectedTripSharingBackend
                                    , noteBackend = selectedNoteBackend
                                    , checklistBackend = selectedChecklistBackend
                                    , databaseConfig = parsedDatabaseConfig
                                    }

parseAuthBackend :: Maybe String -> Either String AuthBackend
parseAuthBackend Nothing = Right AuthBackendFilesystem
parseAuthBackend (Just "filesystem") = Right AuthBackendFilesystem
parseAuthBackend (Just "postgres") = Right AuthBackendPostgres
parseAuthBackend (Just _) = Left "Configuration auth.authBackend must be one of: filesystem, postgres"

parseSessionBackend :: Maybe String -> Either String SessionBackend
parseSessionBackend Nothing = Right SessionBackendFilesystem
parseSessionBackend (Just "filesystem") = Right SessionBackendFilesystem
parseSessionBackend (Just "postgres") = Right SessionBackendPostgres
parseSessionBackend (Just _) = Left "Configuration session.sessionBackend must be one of: filesystem, postgres"

parseCalendarBackend :: Maybe String -> Either String CalendarBackend
parseCalendarBackend Nothing = Right CalendarBackendFilesystem
parseCalendarBackend (Just "filesystem") = Right CalendarBackendFilesystem
parseCalendarBackend (Just "postgres") = Right CalendarBackendPostgres
parseCalendarBackend (Just _) = Left "Configuration calendarBackend must be one of: filesystem, postgres"

parseTripSharingBackend :: Maybe String -> Either String TripSharingBackend
parseTripSharingBackend Nothing = Right TripSharingBackendFilesystem
parseTripSharingBackend (Just "filesystem") = Right TripSharingBackendFilesystem
parseTripSharingBackend (Just "postgres") = Right TripSharingBackendPostgres
parseTripSharingBackend (Just _) = Left "Configuration tripSharingBackend must be one of: filesystem, postgres"

parseNoteBackend :: Maybe String -> Either String NoteBackend
parseNoteBackend Nothing = Right NoteBackendFilesystem
parseNoteBackend (Just "filesystem") = Right NoteBackendFilesystem
parseNoteBackend (Just "postgres") = Right NoteBackendPostgres
parseNoteBackend (Just _) = Left "Configuration noteBackend must be one of: filesystem, postgres"

parseChecklistBackend :: Maybe String -> Either String ChecklistBackend
parseChecklistBackend Nothing = Right ChecklistBackendFilesystem
parseChecklistBackend (Just "filesystem") = Right ChecklistBackendFilesystem
parseChecklistBackend (Just "postgres") = Right ChecklistBackendPostgres
parseChecklistBackend (Just _) = Left "Configuration checklistBackend must be one of: filesystem, postgres"

parseBool :: String -> Maybe Bool
parseBool raw =
  case map toLower raw of
    "true" -> Just True
    "1" -> Just True
    "false" -> Just False
    "0" -> Just False
    _ -> Nothing

runApp :: IO ()
runApp = do
    putStrLn "running server"
    appConfigResult <- loadAppConfigFromFile
    case appConfigResult of
      Left err -> do
        putStrLn $ "[startup-error] " ++ err
        exitFailure
      Right appConfig -> do
        signupRateLimitState <- newMVar []
        tmpDir <- getTemporaryDirectory
        cd <- getCurrentDirectory
        let sessionCfg = sessionConfig appConfig
        let selectedAuthBackend = authBackend appConfig
            selectedSessionBackend = sessionBackend appConfig
            selectedCalendarBackend = calendarBackend appConfig
            selectedTripSharingBackend = tripSharingBackend appConfig
            selectedNoteBackend = noteBackend appConfig
            selectedChecklistBackend = checklistBackend appConfig
        putStrLn ("[startup] auth backend: " ++ renderAuthBackend selectedAuthBackend)
        putStrLn ("[startup] session backend: " ++ renderSessionBackend selectedSessionBackend)
        putStrLn ("[startup] calendar backend: " ++ renderCalendarBackend selectedCalendarBackend)
        putStrLn ("[startup] trip-sharing backend: " ++ renderTripSharingBackend selectedTripSharingBackend)
        putStrLn ("[startup] note backend: " ++ renderNoteBackend selectedNoteBackend)
        putStrLn ("[startup] checklist backend: " ++ renderChecklistBackend selectedChecklistBackend)
        case databaseConfig appConfig of
          Nothing -> pure ()
          Just dbCfg -> putStrLn ("[startup] database target: " ++ renderDatabaseTarget dbCfg)
        migrationsResult <-
          runStartupMigrationsIfNeeded
            selectedAuthBackend
            selectedSessionBackend
            selectedCalendarBackend
            selectedTripSharingBackend
            selectedNoteBackend
            selectedChecklistBackend
            (databaseConfig appConfig)
        case migrationsResult of
          Left err -> do
            putStrLn $ "[startup-error] " ++ err
            exitFailure
          Right () -> pure ()
        sessionStoreResult <- makeSessionStore selectedSessionBackend (databaseConfig appConfig) cd sessionCfg
        case sessionStoreResult of
          Left err -> do
            putStrLn ("[startup] session backend wiring failed for: " ++ renderSessionBackend selectedSessionBackend)
            putStrLn $ "[startup-error] " ++ err
            exitFailure
          Right sessionStore -> do
            putStrLn ("[startup] session backend wiring ready: " ++ renderSessionBackend selectedSessionBackend)
            authRepoResult <- makeAuthRepository selectedAuthBackend (databaseConfig appConfig)
            case authRepoResult of
              Left err -> do
                putStrLn ("[startup] auth backend wiring failed for: " ++ renderAuthBackend selectedAuthBackend)
                putStrLn $ "[startup-error] " ++ err
                exitFailure
              Right authRepo -> do
                putStrLn ("[startup] auth backend wiring ready: " ++ renderAuthBackend selectedAuthBackend)
                authImportResult <- runAuthStartupImportIfNeeded selectedAuthBackend (databaseConfig appConfig)
                case authImportResult of
                  Left err -> do
                    putStrLn $ "[startup-error] " ++ err
                    exitFailure
                  Right () -> do
                    sessionImportResult <- runSessionStartupImportIfNeeded selectedSessionBackend (databaseConfig appConfig) cd
                    case sessionImportResult of
                      Left err -> do
                        putStrLn $ "[startup-error] " ++ err
                        exitFailure
                      Right () -> do
                        calendarRepoResult <- makeCalendarRepository selectedCalendarBackend (databaseConfig appConfig)
                        case calendarRepoResult of
                          Left err -> do
                            putStrLn ("[startup] calendar backend wiring failed for: " ++ renderCalendarBackend selectedCalendarBackend)
                            putStrLn $ "[startup-error] " ++ err
                            exitFailure
                          Right calendarRepo -> do
                            putStrLn ("[startup] calendar backend wiring ready: " ++ renderCalendarBackend selectedCalendarBackend)
                            tripSharingRepoResult <- makeTripSharingRepository selectedTripSharingBackend (databaseConfig appConfig)
                            case tripSharingRepoResult of
                              Left err -> do
                                putStrLn ("[startup] trip-sharing backend wiring failed for: " ++ renderTripSharingBackend selectedTripSharingBackend)
                                putStrLn $ "[startup-error] " ++ err
                                exitFailure
                              Right tripSharingRepo -> do
                                putStrLn ("[startup] trip-sharing backend wiring ready: " ++ renderTripSharingBackend selectedTripSharingBackend)
                                calendarTripSharingImportResult <-
                                  runCalendarTripSharingStartupImportIfNeeded
                                    selectedCalendarBackend
                                    selectedTripSharingBackend
                                    (databaseConfig appConfig)
                                    cd
                                case calendarTripSharingImportResult of
                                  Left err -> do
                                    putStrLn $ "[startup-error] " ++ err
                                    exitFailure
                                  Right () -> do
                                    noteRepoResult <- makeNoteRepository selectedNoteBackend (databaseConfig appConfig)
                                    case noteRepoResult of
                                      Left err -> do
                                        putStrLn ("[startup] note backend wiring failed for: " ++ renderNoteBackend selectedNoteBackend)
                                        putStrLn $ "[startup-error] " ++ err
                                        exitFailure
                                      Right noteRepo -> do
                                        putStrLn ("[startup] note backend wiring ready: " ++ renderNoteBackend selectedNoteBackend)
                                        checklistRepoResult <- makeChecklistRepository selectedChecklistBackend (databaseConfig appConfig)
                                        case checklistRepoResult of
                                          Left err -> do
                                            putStrLn ("[startup] checklist backend wiring failed for: " ++ renderChecklistBackend selectedChecklistBackend)
                                            putStrLn $ "[startup-error] " ++ err
                                            exitFailure
                                          Right checklistRepo -> do
                                            putStrLn ("[startup] checklist backend wiring ready: " ++ renderChecklistBackend selectedChecklistBackend)
                                            notesChecklistImportResult <-
                                              runNotesChecklistStartupImportIfNeeded
                                                selectedNoteBackend
                                                selectedChecklistBackend
                                                (databaseConfig appConfig)
                                                cd
                                            case notesChecklistImportResult of
                                              Left err -> do
                                                putStrLn $ "[startup-error] " ++ err
                                                exitFailure
                                              Right () ->
                                                simpleHTTP nullConf { port = 8081 } $ do
                                                    log "Incoming request" >> log "=========================END REQUEST====================\n"
                                                    msum [ homePage
                                                         , apiController authRepo calendarRepo tripSharingRepo noteRepo checklistRepo signupRateLimitState tmpDir appConfig sessionStore
                                                         , serveStaticResource
                                                         , mzero
                                                         ]

startupMigrationDomainsForBackends
  :: AuthBackend
  -> SessionBackend
  -> CalendarBackend
  -> TripSharingBackend
  -> NoteBackend
  -> ChecklistBackend
  -> [String]
startupMigrationDomainsForBackends authMode sessionMode calendarMode tripSharingMode noteMode checklistMode =
  concat
    [ ["auth" | authMode == AuthBackendPostgres]
    , ["session" | sessionMode == SessionBackendPostgres]
    , ["calendar" | calendarMode == CalendarBackendPostgres]
    , ["trip-sharing" | tripSharingMode == TripSharingBackendPostgres]
    , ["note" | noteMode == NoteBackendPostgres]
    , ["checklist" | checklistMode == ChecklistBackendPostgres]
    ]

runStartupMigrationsIfNeeded
  :: AuthBackend
  -> SessionBackend
  -> CalendarBackend
  -> TripSharingBackend
  -> NoteBackend
  -> ChecklistBackend
  -> Maybe DatabaseConfig
  -> IO (Either String ())
runStartupMigrationsIfNeeded authMode sessionMode calendarMode tripSharingMode noteMode checklistMode mDatabaseCfg = do
  let selectedDomains = startupMigrationDomainsForBackends authMode sessionMode calendarMode tripSharingMode noteMode checklistMode
      skippedDomains = startupMigrationSkippedDomains selectedDomains
      selectedRendered = renderStartupMigrationDomainList selectedDomains
      skippedRendered = renderStartupMigrationDomainList skippedDomains
      selectedCount = length selectedDomains
  putStrLn
    ( "[startup][migrations] start"
        ++ " direction=MigrateUp"
        ++ " selected="
        ++ selectedRendered
        ++ " skipped="
        ++ skippedRendered
    )
  case (selectedDomains, mDatabaseCfg) of
    ([], _) -> do
      putStrLn
        ( "[startup][migrations] completed"
            ++ " direction=MigrateUp"
            ++ " domains=0"
            ++ " selected="
            ++ selectedRendered
            ++ " skipped="
            ++ skippedRendered
        )
      pure (Right ())
    (_, Nothing) -> do
      let err = "Configuration database is required when any backend uses postgres"
      putStrLn
        ( "[startup][migrations] failed"
            ++ " direction=MigrateUp"
            ++ " domain=none"
            ++ " selected="
            ++ selectedRendered
            ++ " skipped="
            ++ skippedRendered
            ++ " reason="
            ++ err
        )
      pure (Left err)
    (_, Just dbCfg) -> do
      let connectionString = renderPostgresConnectionString dbCfg
      runDomains "." connectionString selectedDomains selectedRendered skippedRendered selectedCount
  where
    runDomains _ _ [] selectedRendered skippedRendered selectedCount = do
      putStrLn
        ( "[startup][migrations] completed"
            ++ " direction=MigrateUp"
            ++ " domains="
            ++ show selectedCount
            ++ " selected="
            ++ selectedRendered
            ++ " skipped="
            ++ skippedRendered
        )
      pure (Right ())
    runDomains basePath connectionString (domain:rest) selectedRendered skippedRendered selectedCount = do
      putStrLn ("[startup][migrations] domain=" ++ domain ++ " phase=start direction=MigrateUp")
      result <- runDomainMigration basePath connectionString domain
      case result of
        Left err -> do
          putStrLn
            ( "[startup][migrations] failed"
                ++ " direction=MigrateUp"
                ++ " domain="
                ++ domain
                ++ " selected="
                ++ selectedRendered
                ++ " skipped="
                ++ skippedRendered
                ++ " reason="
                ++ err
            )
          pure (Left err)
        Right () -> do
          putStrLn ("[startup][migrations] domain=" ++ domain ++ " phase=done direction=MigrateUp")
          runDomains basePath connectionString rest selectedRendered skippedRendered selectedCount

startupMigrationSkippedDomains :: [String] -> [String]
startupMigrationSkippedDomains selectedDomains =
  filter (`notElem` selectedDomains) startupMigrationCanonicalDomains

startupMigrationCanonicalDomains :: [String]
startupMigrationCanonicalDomains = ["auth", "session", "calendar", "trip-sharing", "note", "checklist"]

renderStartupMigrationDomainList :: [String] -> String
renderStartupMigrationDomainList [] = "none"
renderStartupMigrationDomainList domains = intercalate "," domains

runDomainMigration :: FilePath -> String -> String -> IO (Either String ())
runDomainMigration basePath connectionString domain = do
  result <-
    case domain of
      "auth" -> runAuthMigrationsAtPath basePath connectionString MigrateUp
      "session" -> runSessionMigrationsAtPath basePath connectionString MigrateUp
      "calendar" -> runCalendarMigrationsAtPath basePath connectionString MigrateUp
      "trip-sharing" -> runTripSharingMigrationsAtPath basePath connectionString MigrateUp
      "note" -> runNoteMigrationsAtPath basePath connectionString MigrateUp
      "checklist" -> runChecklistMigrationsAtPath basePath connectionString MigrateUp
      _ -> pure (Left ("Unsupported migration domain: " ++ domain))
  case result of
    Left err -> pure (Left ("Startup migrations failed for domain=" ++ domain ++ ": " ++ err))
    Right () -> pure (Right ())

makeAuthRepository :: AuthBackend -> Maybe DatabaseConfig -> IO (Either String AuthRepository)
makeAuthRepository AuthBackendFilesystem _ = pure (Right defaultAuthRepository)
makeAuthRepository AuthBackendPostgres mDatabaseCfg =
  case mDatabaseCfg of
    Nothing -> pure (Left "Configuration database is required when auth.authBackend=postgres")
    Just dbCfg -> do
      let connectionString = renderPostgresConnectionString dbCfg
      validationResult <- AuthRepository.verifyPostgresAuthStorage connectionString
      case validationResult of
        Left err -> pure (Left ("Postgres auth storage validation failed: " ++ err))
        Right () -> pure (Right (AuthRepository.postgresAuthRepository connectionString))

runAuthStartupImportIfNeeded :: AuthBackend -> Maybe DatabaseConfig -> IO (Either String ())
runAuthStartupImportIfNeeded AuthBackendFilesystem _ = pure (Right ())
runAuthStartupImportIfNeeded AuthBackendPostgres Nothing =
  pure (Left "Configuration database is required when auth.authBackend=postgres")
runAuthStartupImportIfNeeded AuthBackendPostgres (Just dbCfg) = do
  let connectionString = renderPostgresConnectionString dbCfg
      filesystemRepo = defaultAuthRepository
      postgresRepo = AuthRepository.postgresAuthRepository connectionString
  filesystemUsersResult <- runExceptT (AuthRepository.repoListUsers filesystemRepo)
  filesystemUsers <-
    case filesystemUsersResult of
      Right users -> pure (Right users)
      Left StorageFailure -> do
        putStrLn "[startup][auth-import] source users directory is missing; treating filesystem auth source as empty"
        pure (Right [])
      Left err -> pure (Left ("Auth startup import failed while reading filesystem users: " ++ show err))
  case filesystemUsers of
    Left err -> pure (Left err)
    Right fsUsers -> do
      postgresUsersResult <- runExceptT (AuthRepository.repoListUsers postgresRepo)
      case postgresUsersResult of
        Left err -> pure (Left ("Auth startup import failed while reading Postgres users: " ++ show err))
        Right pgUsers -> do
          let orderedFsUsers = sortOn AuthRepository.uname fsUsers
              pgUsernames = Set.fromList (map AuthRepository.uname pgUsers)
          when (not (null orderedFsUsers) && not (null pgUsers)) $
            putStrLn ("[startup][auth-import][warning] overlap detected: filesystem_count=" ++ show (length orderedFsUsers) ++ " postgres_count=" ++ show (length pgUsers) ++ " conflict_policy=postgres-wins")
          importResult <- foldM (importSingleAuthUser postgresRepo pgUsernames) (Right (0 :: Int, 0 :: Int)) orderedFsUsers
          case importResult of
            Left err -> pure (Left err)
            Right (importedCount, skippedCount) -> do
              putStrLn ("[startup][auth-import] completed filesystem_count=" ++ show (length orderedFsUsers) ++ " postgres_count=" ++ show (length pgUsers) ++ " imported=" ++ show importedCount ++ " skipped_conflicts=" ++ show skippedCount)
              pure (Right ())

importSingleAuthUser :: AuthRepository -> Set.Set String -> Either String (Int, Int) -> AuthRepository.PersistedUser -> IO (Either String (Int, Int))
importSingleAuthUser _ _ (Left err) _ = pure (Left err)
importSingleAuthUser postgresRepo pgUsernames (Right (importedCount, skippedCount)) fsUser =
  if Set.member username pgUsernames
    then do
      putStrLn ("[startup][auth-import][warning] skipping conflicting username=" ++ username ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1))
    else do
      createResult <- runExceptT (AuthRepository.repoCreateUser postgresRepo fsUser)
      case createResult of
        Right () -> pure (Right (importedCount + 1, skippedCount))
        Left AlreadyExists -> do
          putStrLn ("[startup][auth-import][warning] skipping conflicting username=" ++ username ++ " policy=postgres-wins")
          pure (Right (importedCount, skippedCount + 1))
        Left err -> pure (Left ("Auth startup import failed while writing username=" ++ username ++ ": " ++ show err))
  where
    username = AuthRepository.uname fsUser

runSessionStartupImportIfNeeded :: SessionBackend -> Maybe DatabaseConfig -> FilePath -> IO (Either String ())
runSessionStartupImportIfNeeded SessionBackendFilesystem _ _ = pure (Right ())
runSessionStartupImportIfNeeded SessionBackendPostgres Nothing _ =
  pure (Left "Configuration database is required when session.sessionBackend=postgres")
runSessionStartupImportIfNeeded SessionBackendPostgres (Just dbCfg) cd = do
  let connectionString = renderPostgresConnectionString dbCfg
      sessionBaseDir = cd </> "data" </> "sessions"
      postgresRepo = mkPostgresSessionRepository connectionString
  filesystemSourceResult <- loadFilesystemSessionImportSource sessionBaseDir
  case filesystemSourceResult of
    Left err -> pure (Left err)
    Right (fsStates, fsHandles, fsBindings) -> do
      postgresSnapshotResult <- loadPostgresSessionImportSnapshot connectionString
      case postgresSnapshotResult of
        Left err -> pure (Left err)
        Right (pgStates, pgHandles, pgBindings) -> do
          let orderedFsStates = sortOn stateId fsStates
              orderedFsHandles = sortOn handleSessionId fsHandles
              orderedFsBindings = sortOn fst fsBindings
              pgStateIds = Set.fromList (map stateId pgStates)
              pgSessionIds = Set.fromList (map handleSessionId pgHandles)
              pgBindingUserIds = Set.fromList (map fst pgBindings)
              filesystemCount = length orderedFsStates + length orderedFsHandles + length orderedFsBindings
              postgresCount = length pgStates + length pgHandles + length pgBindings
          when (filesystemCount > 0 && postgresCount > 0) $
            putStrLn
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
          statesImportResult <- foldM (importSingleSessionState postgresRepo) (Right (0 :: Int, 0 :: Int, pgStateIds)) orderedFsStates
          case statesImportResult of
            Left err -> pure (Left err)
            Right (statesImported, statesSkipped, _) -> do
              handlesImportResult <- foldM (importSingleSessionHandle postgresRepo) (Right (0 :: Int, 0 :: Int, pgSessionIds)) orderedFsHandles
              case handlesImportResult of
                Left err -> pure (Left err)
                Right (handlesImported, handlesSkipped, _) -> do
                  bindingsImportResult <- foldM (importSingleSessionUserBinding postgresRepo) (Right (0 :: Int, 0 :: Int, pgBindingUserIds)) orderedFsBindings
                  case bindingsImportResult of
                    Left err -> pure (Left err)
                    Right (bindingsImported, bindingsSkipped, _) -> do
                      putStrLn
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
                      pure (Right ())

importSingleSessionState
  :: SessionRepository
  -> Either String (Int, Int, Set.Set String)
  -> SessionState
  -> IO (Either String (Int, Int, Set.Set String))
importSingleSessionState _ (Left err) _ = pure (Left err)
importSingleSessionState postgresRepo (Right (importedCount, skippedCount, knownStateIds)) sessionState =
  if Set.member stateKey knownStateIds
    then do
      putStrLn ("[startup][session-import][warning] skipping conflicting state_id=" ++ stateKey ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownStateIds))
    else do
      createResult <- runExceptT (repoCreateSessionState postgresRepo sessionState)
      case createResult of
        Right () ->
          pure (Right (importedCount + 1, skippedCount, Set.insert stateKey knownStateIds))
        Left AlreadyExists -> do
          putStrLn ("[startup][session-import][warning] skipping conflicting state_id=" ++ stateKey ++ " policy=postgres-wins")
          pure (Right (importedCount, skippedCount + 1, Set.insert stateKey knownStateIds))
        Left err ->
          pure (Left ("Session startup import failed while writing state_id=" ++ stateKey ++ ": " ++ show err))
  where
    stateKey = stateId sessionState

importSingleSessionHandle
  :: SessionRepository
  -> Either String (Int, Int, Set.Set String)
  -> SessionHandle
  -> IO (Either String (Int, Int, Set.Set String))
importSingleSessionHandle _ (Left err) _ = pure (Left err)
importSingleSessionHandle postgresRepo (Right (importedCount, skippedCount, knownSessionIds)) sessionHandle =
  if Set.member sessionKey knownSessionIds
    then do
      putStrLn ("[startup][session-import][warning] skipping conflicting session_id=" ++ sessionKey ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownSessionIds))
    else do
      createResult <- runExceptT (repoCreateSessionHandle postgresRepo sessionHandle)
      case createResult of
        Right () ->
          pure (Right (importedCount + 1, skippedCount, Set.insert sessionKey knownSessionIds))
        Left AlreadyExists -> do
          putStrLn ("[startup][session-import][warning] skipping conflicting session_id=" ++ sessionKey ++ " policy=postgres-wins")
          pure (Right (importedCount, skippedCount + 1, Set.insert sessionKey knownSessionIds))
        Left err ->
          pure (Left ("Session startup import failed while writing session_id=" ++ sessionKey ++ ": " ++ show err))
  where
    sessionKey = handleSessionId sessionHandle

importSingleSessionUserBinding
  :: SessionRepository
  -> Either String (Int, Int, Set.Set String)
  -> (String, UserStateBinding)
  -> IO (Either String (Int, Int, Set.Set String))
importSingleSessionUserBinding _ (Left err) _ = pure (Left err)
importSingleSessionUserBinding postgresRepo (Right (importedCount, skippedCount, knownUserIds)) (userId, binding) =
  if Set.member userId knownUserIds
    then do
      putStrLn ("[startup][session-import][warning] skipping conflicting user_id=" ++ userId ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownUserIds))
    else do
      createResult <- runExceptT (repoCreateUserStateBinding postgresRepo userId binding)
      case createResult of
        Right () ->
          pure (Right (importedCount + 1, skippedCount, Set.insert userId knownUserIds))
        Left AlreadyExists -> do
          putStrLn ("[startup][session-import][warning] skipping conflicting user_id=" ++ userId ++ " policy=postgres-wins")
          pure (Right (importedCount, skippedCount + 1, Set.insert userId knownUserIds))
        Left err ->
          pure (Left ("Session startup import failed while writing user_id=" ++ userId ++ ": " ++ show err))

loadFilesystemSessionImportSource :: FilePath -> IO (Either String ([SessionState], [SessionHandle], [(String, UserStateBinding)]))
loadFilesystemSessionImportSource baseDir = do
  baseExists <- doesDirectoryExist baseDir
  if not baseExists
    then do
      putStrLn "[startup][session-import] source sessions directory is missing; treating filesystem session source as empty"
      pure (Right ([], [], []))
    else do
      statesResult <- decodeJsonDirectory (baseDir </> "states")
      case statesResult of
        Left err -> pure (Left ("Session startup import failed while reading filesystem states: " ++ err))
        Right states -> do
          handlesResult <- decodeJsonDirectory (baseDir </> "handles")
          case handlesResult of
            Left err -> pure (Left ("Session startup import failed while reading filesystem handles: " ++ err))
            Right handles -> do
              bindingsResult <- decodeSessionBindingsDirectory (baseDir </> "users")
              case bindingsResult of
                Left err -> pure (Left ("Session startup import failed while reading filesystem user bindings: " ++ err))
                Right bindings -> pure (Right (states, handles, bindings))

loadPostgresSessionImportSnapshot :: String -> IO (Either String ([SessionState], [SessionHandle], [(String, UserStateBinding)]))
loadPostgresSessionImportSnapshot connectionString = do
  connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
  case connResult of
    Left err -> pure (Left ("Session startup import failed while connecting to Postgres: " ++ show err))
    Right conn -> do
      statesResult <- Ex.try (query_ conn "SELECT state_id::text, user_id, created_at, expires_at, idle_expires_at, revoked_at FROM session_states" :: IO [(String, String, UTCTime, UTCTime, UTCTime, Maybe UTCTime)])
      handlesResult <- Ex.try (query_ conn "SELECT session_id::text, state_id::text, issued_at, revoked_at FROM session_handles" :: IO [(String, String, UTCTime, Maybe UTCTime)])
      bindingsResult <- Ex.try (query_ conn "SELECT user_id, state_id::text FROM session_user_bindings" :: IO [(String, String)])
      _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
      case statesResult of
        Left err -> pure (Left ("Session startup import failed while reading Postgres states: " ++ show (err :: Ex.SomeException)))
        Right stateRows ->
          case handlesResult of
            Left err -> pure (Left ("Session startup import failed while reading Postgres handles: " ++ show (err :: Ex.SomeException)))
            Right handleRows ->
              case bindingsResult of
                Left err -> pure (Left ("Session startup import failed while reading Postgres user bindings: " ++ show (err :: Ex.SomeException)))
                Right bindingRows ->
                  pure
                        ( Right
                        ( map (\(sid, uid, createdAt, expiresAt, idleExpiresAt, revokedAt) -> SessionState sid uid createdAt expiresAt idleExpiresAt revokedAt) stateRows
                        , map (\(sessionId, stId, issuedAt, revokedAt) -> SessionHandle sessionId stId issuedAt revokedAt) handleRows
                        , map (second UserStateBinding) bindingRows
                        )
                    )

decodeJsonDirectory :: FromJSON a => FilePath -> IO (Either String [a])
decodeJsonDirectory dirPath = do
  exists <- doesDirectoryExist dirPath
  if not exists
    then pure (Right [])
    else do
      files <- listDirectory dirPath
      foldM loadFile (Right []) (sortOn id files)
  where
    loadFile (Left err) _ = pure (Left err)
    loadFile (Right acc) fileName
      | takeExtension fileName /= ".json" = pure (Right acc)
      | otherwise = do
          let fullPath = dirPath </> fileName
          content <- BL.readFile fullPath
          case decode content of
            Nothing -> pure (Left ("invalid JSON in " ++ fullPath))
            Just parsed -> pure (Right (parsed : acc))

decodeSessionBindingsDirectory :: FilePath -> IO (Either String [(String, UserStateBinding)])
decodeSessionBindingsDirectory dirPath = do
  exists <- doesDirectoryExist dirPath
  if not exists
    then pure (Right [])
    else do
      files <- listDirectory dirPath
      foldM loadFile (Right []) (sortOn id files)
  where
    loadFile (Left err) _ = pure (Left err)
    loadFile (Right acc) fileName
      | takeExtension fileName /= ".json" = pure (Right acc)
      | otherwise = do
          let fullPath = dirPath </> fileName
              userId = takeBaseName fileName
          content <- BL.readFile fullPath
          case decode content of
            Nothing -> pure (Left ("invalid JSON in " ++ fullPath))
            Just parsed -> pure (Right ((userId, parsed) : acc))

runCalendarTripSharingStartupImportIfNeeded
  :: CalendarBackend
  -> TripSharingBackend
  -> Maybe DatabaseConfig
  -> FilePath
  -> IO (Either String ())
runCalendarTripSharingStartupImportIfNeeded calendarMode tripSharingMode _ _
  | calendarMode == CalendarBackendFilesystem
      && tripSharingMode == TripSharingBackendFilesystem =
      pure (Right ())
runCalendarTripSharingStartupImportIfNeeded CalendarBackendPostgres _ Nothing _ =
  pure (Left "Configuration database is required when calendarBackend=postgres")
runCalendarTripSharingStartupImportIfNeeded _ TripSharingBackendPostgres Nothing _ =
  pure (Left "Configuration database is required when tripSharingBackend=postgres")
runCalendarTripSharingStartupImportIfNeeded calendarMode tripSharingMode (Just dbCfg) cd = do
  let connectionString = renderPostgresConnectionString dbCfg
  calendarResult <-
    case calendarMode of
      CalendarBackendFilesystem -> pure (Right ())
      CalendarBackendPostgres -> runCalendarStartupImport connectionString cd
  case calendarResult of
    Left err -> pure (Left err)
    Right () ->
      case tripSharingMode of
        TripSharingBackendFilesystem -> pure (Right ())
        TripSharingBackendPostgres -> runTripSharingStartupImport connectionString cd

runNotesChecklistStartupImportIfNeeded
  :: NoteBackend
  -> ChecklistBackend
  -> Maybe DatabaseConfig
  -> FilePath
  -> IO (Either String ())
runNotesChecklistStartupImportIfNeeded noteMode checklistMode _ _
  | noteMode == NoteBackendFilesystem
      && checklistMode == ChecklistBackendFilesystem =
      pure (Right ())
runNotesChecklistStartupImportIfNeeded NoteBackendPostgres _ Nothing _ =
  pure (Left "Configuration database is required when noteBackend=postgres")
runNotesChecklistStartupImportIfNeeded _ ChecklistBackendPostgres Nothing _ =
  pure (Left "Configuration database is required when checklistBackend=postgres")
runNotesChecklistStartupImportIfNeeded noteMode checklistMode (Just dbCfg) cd = do
  let connectionString = renderPostgresConnectionString dbCfg
  noteResult <-
    case noteMode of
      NoteBackendFilesystem -> pure (Right ())
      NoteBackendPostgres -> runNoteStartupImport connectionString cd
  case noteResult of
    Left err -> pure (Left err)
    Right () ->
      case checklistMode of
        ChecklistBackendFilesystem -> pure (Right ())
        ChecklistBackendPostgres -> runChecklistStartupImport connectionString cd

runNoteStartupImport :: String -> FilePath -> IO (Either String ())
runNoteStartupImport connectionString cd = do
  let notesBaseDir = cd </> "data" </> "note"
  filesystemEntriesResult <- loadFilesystemNoteImportEntries notesBaseDir
  case filesystemEntriesResult of
    Left err -> pure (Left err)
    Right fsEntries -> do
      connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
      case connResult of
        Left err -> pure (Left ("Note startup import failed while connecting to Postgres: " ++ show err))
        Right conn -> do
          snapshotResult <- Ex.try (query_ conn "SELECT item_id FROM note_items" :: IO [Only String]) :: IO (Either Ex.SomeException [Only String])
          case snapshotResult of
            Left err -> do
              _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
              pure (Left ("Note startup import failed while reading Postgres notes: " ++ show err))
            Right pgRows -> do
              let orderedFsEntries = sortOn (\(itemId, _, _) -> itemId) fsEntries
                  knownIds = Set.fromList (map fromOnly pgRows)
              when (not (null orderedFsEntries) && not (null pgRows)) $
                putStrLn
                  ( "[startup][note-import][warning] overlap detected:"
                      ++ " filesystem_count="
                      ++ show (length orderedFsEntries)
                      ++ " postgres_count="
                      ++ show (length pgRows)
                      ++ " conflict_policy=postgres-wins"
                  )
              importResult <- foldM (importSingleNoteItem conn) (Right (0 :: Int, 0 :: Int, knownIds)) orderedFsEntries
              _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
              case importResult of
                Left err -> pure (Left err)
                Right (importedCount, skippedCount, _) -> do
                  putStrLn
                    ( "[startup][note-import] completed"
                        ++ " filesystem_count="
                        ++ show (length orderedFsEntries)
                        ++ " postgres_count="
                        ++ show (length pgRows)
                        ++ " imported="
                        ++ show importedCount
                        ++ " skipped_conflicts="
                        ++ show skippedCount
                    )
                  pure (Right ())

runChecklistStartupImport :: String -> FilePath -> IO (Either String ())
runChecklistStartupImport connectionString cd = do
  let checklistsBaseDir = cd </> "data" </> "checklist"
  filesystemEntriesResult <- loadFilesystemChecklistImportEntries checklistsBaseDir
  case filesystemEntriesResult of
    Left err -> pure (Left err)
    Right fsEntries -> do
      connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
      case connResult of
        Left err -> pure (Left ("Checklist startup import failed while connecting to Postgres: " ++ show err))
        Right conn -> do
          snapshotResult <- Ex.try (query_ conn "SELECT item_id FROM checklist_items" :: IO [Only String]) :: IO (Either Ex.SomeException [Only String])
          case snapshotResult of
            Left err -> do
              _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
              pure (Left ("Checklist startup import failed while reading Postgres checklists: " ++ show err))
            Right pgRows -> do
              let orderedFsEntries = sortOn (\(itemId, _, _) -> itemId) fsEntries
                  knownIds = Set.fromList (map fromOnly pgRows)
              when (not (null orderedFsEntries) && not (null pgRows)) $
                putStrLn
                  ( "[startup][checklist-import][warning] overlap detected:"
                      ++ " filesystem_count="
                      ++ show (length orderedFsEntries)
                      ++ " postgres_count="
                      ++ show (length pgRows)
                      ++ " conflict_policy=postgres-wins"
                  )
              importResult <- foldM (importSingleChecklistItem conn) (Right (0 :: Int, 0 :: Int, knownIds)) orderedFsEntries
              _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
              case importResult of
                Left err -> pure (Left err)
                Right (importedCount, skippedCount, _) -> do
                  putStrLn
                    ( "[startup][checklist-import] completed"
                        ++ " filesystem_count="
                        ++ show (length orderedFsEntries)
                        ++ " postgres_count="
                        ++ show (length pgRows)
                        ++ " imported="
                        ++ show importedCount
                        ++ " skipped_conflicts="
                        ++ show skippedCount
                    )
                  pure (Right ())

importSingleNoteItem
  :: Connection
  -> Either String (Int, Int, Set.Set String)
  -> (String, String, NoteContent)
  -> IO (Either String (Int, Int, Set.Set String))
importSingleNoteItem _ (Left err) _ = pure (Left err)
importSingleNoteItem conn (Right (importedCount, skippedCount, knownIds)) (itemId, itemVersion, noteContent) =
  if Set.member itemId knownIds
    then do
      putStrLn ("[startup][note-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownIds))
    else do
      writeResult <- Ex.try
        (execute conn
          "INSERT INTO note_items (item_id, item_version, item_content) VALUES (?, ?, ?::jsonb)"
          (itemId, itemVersion, BL8.unpack (encode noteContent)))
        :: IO (Either Ex.SomeException Int64)
      case writeResult of
        Right _ ->
          pure (Right (importedCount + 1, skippedCount, Set.insert itemId knownIds))
        Left err ->
          case Ex.fromException err of
            Just sqlErr
              | isUniqueViolation sqlErr -> do
                  putStrLn ("[startup][note-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
                  pure (Right (importedCount, skippedCount + 1, Set.insert itemId knownIds))
            _ ->
              pure (Left ("Note startup import failed while writing item_id=" ++ itemId ++ ": " ++ show err))

importSingleChecklistItem
  :: Connection
  -> Either String (Int, Int, Set.Set String)
  -> (String, String, ChecklistContent)
  -> IO (Either String (Int, Int, Set.Set String))
importSingleChecklistItem _ (Left err) _ = pure (Left err)
importSingleChecklistItem conn (Right (importedCount, skippedCount, knownIds)) (itemId, itemVersion, checklistContent) =
  if Set.member itemId knownIds
    then do
      putStrLn ("[startup][checklist-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownIds))
    else do
      writeResult <- Ex.try
        (execute conn
          "INSERT INTO checklist_items (item_id, item_version, item_content) VALUES (?, ?, ?::jsonb)"
          (itemId, itemVersion, BL8.unpack (encode checklistContent)))
        :: IO (Either Ex.SomeException Int64)
      case writeResult of
        Right _ ->
          pure (Right (importedCount + 1, skippedCount, Set.insert itemId knownIds))
        Left err ->
          case Ex.fromException err of
            Just sqlErr
              | isUniqueViolation sqlErr -> do
                  putStrLn ("[startup][checklist-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
                  pure (Right (importedCount, skippedCount + 1, Set.insert itemId knownIds))
            _ ->
              pure (Left ("Checklist startup import failed while writing item_id=" ++ itemId ++ ": " ++ show err))

loadFilesystemNoteImportEntries :: FilePath -> IO (Either String [(String, String, NoteContent)])
loadFilesystemNoteImportEntries rootDir = do
  exists <- doesDirectoryExist rootDir
  if not exists
    then do
      putStrLn "[startup][note-import] source note directory is missing; treating filesystem note source as empty"
      pure (Right [])
    else do
      files <- listDirectory rootDir
      foldM (decodeSingleFilesystemNote rootDir) (Right []) (sort files)

decodeSingleFilesystemNote
  :: FilePath
  -> Either String [(String, String, NoteContent)]
  -> FilePath
  -> IO (Either String [(String, String, NoteContent)])
decodeSingleFilesystemNote _ (Left err) _ = pure (Left err)
decodeSingleFilesystemNote rootDir (Right acc) fileName
  | takeExtension fileName /= ".txt" = pure (Right acc)
  | otherwise = do
      let fullPath = rootDir </> fileName
      contentResult <- Ex.try (BL.readFile fullPath) :: IO (Either Ex.IOException BL.ByteString)
      case contentResult of
        Left err ->
          pure (Left ("Note startup import failed while reading filesystem note file " ++ fullPath ++ ": " ++ show err))
        Right raw ->
          case decode raw of
            Nothing ->
              pure (Left ("Note startup import failed while reading filesystem notes: invalid JSON in " ++ fullPath))
            Just noteItem ->
              let itemStorageId = storageId (noteItem :: Identifiable NoteContent)
                  itemId = Model.id itemStorageId
                  itemVersion = Model.version itemStorageId
               in pure (Right ((itemId, itemVersion, content noteItem) : acc))

loadFilesystemChecklistImportEntries :: FilePath -> IO (Either String [(String, String, ChecklistContent)])
loadFilesystemChecklistImportEntries rootDir = do
  exists <- doesDirectoryExist rootDir
  if not exists
    then do
      putStrLn "[startup][checklist-import] source checklist directory is missing; treating filesystem checklist source as empty"
      pure (Right [])
    else do
      files <- listDirectory rootDir
      foldM (decodeSingleFilesystemChecklist rootDir) (Right []) (sort files)

decodeSingleFilesystemChecklist
  :: FilePath
  -> Either String [(String, String, ChecklistContent)]
  -> FilePath
  -> IO (Either String [(String, String, ChecklistContent)])
decodeSingleFilesystemChecklist _ (Left err) _ = pure (Left err)
decodeSingleFilesystemChecklist rootDir (Right acc) fileName
  | takeExtension fileName /= ".txt" = pure (Right acc)
  | otherwise = do
      let fullPath = rootDir </> fileName
      contentResult <- Ex.try (BL.readFile fullPath) :: IO (Either Ex.IOException BL.ByteString)
      case contentResult of
        Left err ->
          pure (Left ("Checklist startup import failed while reading filesystem checklist file " ++ fullPath ++ ": " ++ show err))
        Right raw ->
          case decode raw of
            Nothing ->
              pure (Left ("Checklist startup import failed while reading filesystem checklists: invalid JSON in " ++ fullPath))
            Just checklistItem ->
              let itemStorageId = storageId (checklistItem :: Identifiable ChecklistContent)
                  itemId = Model.id itemStorageId
                  itemVersion = Model.version itemStorageId
               in pure (Right ((itemId, itemVersion, content checklistItem) : acc))

runCalendarStartupImport :: String -> FilePath -> IO (Either String ())
runCalendarStartupImport connectionString cd = do
  let calendarBaseDir = cd </> "data" </> "calendar-items"
  filesystemEntriesResult <- loadFilesystemCalendarImportEntries calendarBaseDir
  case filesystemEntriesResult of
    Left err -> pure (Left err)
    Right fsEntries -> do
      connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
      case connResult of
        Left err -> pure (Left ("Calendar startup import failed while connecting to Postgres: " ++ show err))
        Right conn -> do
          snapshotResult <- Ex.try (query_ conn "SELECT user_id, item_id FROM calendar_items" :: IO [(String, String)]) :: IO (Either Ex.SomeException [(String, String)])
          case snapshotResult of
            Left err -> do
              _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
              pure (Left ("Calendar startup import failed while reading Postgres items: " ++ show err))
            Right pgRows -> do
              let orderedFsEntries = sortOn (\(userId, itemId, _) -> (userId, itemId)) fsEntries
                  knownKeys = Set.fromList pgRows
              when (not (null orderedFsEntries) && not (null pgRows)) $
                putStrLn
                  ( "[startup][calendar-import][warning] overlap detected:"
                      ++ " filesystem_count="
                      ++ show (length orderedFsEntries)
                      ++ " postgres_count="
                      ++ show (length pgRows)
                      ++ " conflict_policy=postgres-wins"
                  )
              importResult <- foldM (importSingleCalendarItem conn) (Right (0 :: Int, 0 :: Int, knownKeys)) orderedFsEntries
              _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
              case importResult of
                Left err -> pure (Left err)
                Right (importedCount, skippedCount, _) -> do
                  putStrLn
                    ( "[startup][calendar-import] completed"
                        ++ " filesystem_count="
                        ++ show (length orderedFsEntries)
                        ++ " postgres_count="
                        ++ show (length pgRows)
                        ++ " imported="
                        ++ show importedCount
                        ++ " skipped_conflicts="
                        ++ show skippedCount
                    )
                  pure (Right ())

importSingleCalendarItem
  :: Connection
  -> Either String (Int, Int, Set.Set (String, String))
  -> (String, String, Agenda.CalendarItemContent)
  -> IO (Either String (Int, Int, Set.Set (String, String)))
importSingleCalendarItem _ (Left err) _ = pure (Left err)
importSingleCalendarItem conn (Right (importedCount, skippedCount, knownKeys)) (userId, itemId, content) = do
  let key = (userId, itemId)
  if Set.member key knownKeys
    then do
      putStrLn ("[startup][calendar-import][warning] skipping conflicting user_id=" ++ userId ++ " item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownKeys))
    else do
      let row = calendarContentToDbRow content
      writeResult <- Ex.try
        (execute conn
          "INSERT INTO calendar_items (user_id, item_id, item_kind, legacy_item_type, legacy_title, legacy_window_start, legacy_window_end, legacy_status, legacy_source_item_id, legacy_actual_duration_minutes, legacy_category, legacy_recurrence_rule_type, legacy_recurrence_interval_days, legacy_recurrence_exception_dates, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
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
        :: IO (Either Ex.SomeException Int64)
      case writeResult of
        Right _ -> pure (Right (importedCount + 1, skippedCount, Set.insert key knownKeys))
        Left err ->
          case Ex.fromException err of
            Just sqlErr
              | isUniqueViolation sqlErr -> do
                  putStrLn ("[startup][calendar-import][warning] skipping conflicting user_id=" ++ userId ++ " item_id=" ++ itemId ++ " policy=postgres-wins")
                  pure (Right (importedCount, skippedCount + 1, Set.insert key knownKeys))
            _ ->
              pure (Left ("Calendar startup import failed while writing user_id=" ++ userId ++ " item_id=" ++ itemId ++ ": " ++ show err))

loadFilesystemCalendarImportEntries :: FilePath -> IO (Either String [(String, String, Agenda.CalendarItemContent)])
loadFilesystemCalendarImportEntries calendarBaseDir = do
  baseExists <- doesDirectoryExist calendarBaseDir
  if not baseExists
    then do
      putStrLn "[startup][calendar-import] source calendar directory is missing; treating filesystem calendar source as empty"
      pure (Right [])
    else do
      entries <- listDirectory calendarBaseDir
      foldM (loadSingleCalendarUserDirectory calendarBaseDir) (Right []) (sort entries)

loadSingleCalendarUserDirectory
  :: FilePath
  -> Either String [(String, String, Agenda.CalendarItemContent)]
  -> FilePath
  -> IO (Either String [(String, String, Agenda.CalendarItemContent)])
loadSingleCalendarUserDirectory _ (Left err) _ = pure (Left err)
loadSingleCalendarUserDirectory calendarBaseDir (Right acc) userId = do
  let userDir = calendarBaseDir </> userId
  isDir <- doesDirectoryExist userDir
  if not isDir
    then pure (Right acc)
    else do
      itemsResult <- decodeJsonDirectory userDir :: IO (Either String [Agenda.CalendarItem])
      case itemsResult of
        Left err ->
          pure (Left ("Calendar startup import failed while reading filesystem items for user_id=" ++ userId ++ ": " ++ err))
        Right items ->
          foldM
            (extractCalendarImportEntry userId)
            (Right acc)
            items

extractCalendarImportEntry
  :: String
  -> Either String [(String, String, Agenda.CalendarItemContent)]
  -> Agenda.CalendarItem
  -> IO (Either String [(String, String, Agenda.CalendarItemContent)])
extractCalendarImportEntry _ (Left err) _ = pure (Left err)
extractCalendarImportEntry userId (Right acc) item =
  case item of
    Agenda.ServerCalendarItem {Agenda.itemId, Agenda.content} ->
      pure (Right ((userId, itemId, content) : acc))
    Agenda.NewCalendarItem {} ->
      pure (Left ("Calendar startup import failed while reading filesystem items for user_id=" ++ userId ++ ": expected stored calendar item with id"))

runTripSharingStartupImport :: String -> FilePath -> IO (Either String ())
runTripSharingStartupImport connectionString cd = do
  let sharesBaseDir = cd </> "data" </> "trip-sharing" </> "shares"
      subscriptionsBaseDir = cd </> "data" </> "trip-sharing" </> "subscriptions"
  sharesResult <- loadFilesystemOwnerUserPairs sharesBaseDir "shares"
  case sharesResult of
    Left err -> pure (Left err)
    Right fsShares -> do
      subscriptionsResult <- loadFilesystemOwnerUserPairs subscriptionsBaseDir "subscriptions"
      case subscriptionsResult of
        Left err -> pure (Left err)
        Right fsSubscriptions -> do
          connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
          case connResult of
            Left err -> pure (Left ("Trip-sharing startup import failed while connecting to Postgres: " ++ show err))
            Right conn -> do
              pgSharesResult <- Ex.try (query_ conn "SELECT owner_user_id, target_username FROM trip_shares" :: IO [(String, String)]) :: IO (Either Ex.SomeException [(String, String)])
              pgSubscriptionsResult <- Ex.try (query_ conn "SELECT owner_user_id, target_username FROM trip_subscriptions" :: IO [(String, String)]) :: IO (Either Ex.SomeException [(String, String)])
              case pgSharesResult of
                Left err -> do
                  _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
                  pure (Left ("Trip-sharing startup import failed while reading Postgres shares: " ++ show err))
                Right pgShares ->
                  case pgSubscriptionsResult of
                    Left err -> do
                      _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
                      pure (Left ("Trip-sharing startup import failed while reading Postgres subscriptions: " ++ show err))
                    Right pgSubscriptions -> do
                      let orderedFsShares = sortOn id fsShares
                          orderedFsSubscriptions = sortOn id fsSubscriptions
                          pgShareKeys = Set.fromList pgShares
                          pgSubscriptionKeys = Set.fromList pgSubscriptions
                          filesystemCount = length orderedFsShares + length orderedFsSubscriptions
                          postgresCount = length pgShares + length pgSubscriptions
                      when (filesystemCount > 0 && postgresCount > 0) $
                        putStrLn
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
                      sharesImportResult <- foldM (importSingleTripShare conn) (Right (0 :: Int, 0 :: Int, pgShareKeys)) orderedFsShares
                      case sharesImportResult of
                        Left err -> do
                          _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
                          pure (Left err)
                        Right (sharesImported, sharesSkipped, _) -> do
                          subscriptionsImportResult <- foldM (importSingleTripSubscription conn) (Right (0 :: Int, 0 :: Int, pgSubscriptionKeys)) orderedFsSubscriptions
                          _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
                          case subscriptionsImportResult of
                            Left err -> pure (Left err)
                            Right (subscriptionsImported, subscriptionsSkipped, _) -> do
                              putStrLn
                                ( "[startup][trip-sharing-import] completed"
                                    ++ " filesystem_shares="
                                    ++ show (length orderedFsShares)
                                    ++ " filesystem_subscriptions="
                                    ++ show (length orderedFsSubscriptions)
                                    ++ " postgres_shares="
                                    ++ show (length pgShares)
                                    ++ " postgres_subscriptions="
                                    ++ show (length pgSubscriptions)
                                    ++ " imported_shares="
                                    ++ show sharesImported
                                    ++ " imported_subscriptions="
                                    ++ show subscriptionsImported
                                    ++ " skipped_share_conflicts="
                                    ++ show sharesSkipped
                                    ++ " skipped_subscription_conflicts="
                                    ++ show subscriptionsSkipped
                                )
                              pure (Right ())

importSingleTripShare
  :: Connection
  -> Either String (Int, Int, Set.Set (String, String))
  -> (String, String)
  -> IO (Either String (Int, Int, Set.Set (String, String)))
importSingleTripShare _ (Left err) _ = pure (Left err)
importSingleTripShare conn (Right (importedCount, skippedCount, knownKeys)) (ownerUserId, targetUsername) =
  if Set.member (ownerUserId, targetUsername) knownKeys
    then do
      putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting share owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownKeys))
    else do
      writeResult <- Ex.try
        (execute conn
          "INSERT INTO trip_shares (owner_user_id, target_username) VALUES (?, ?)"
          (ownerUserId, targetUsername))
        :: IO (Either Ex.SomeException Int64)
      case writeResult of
        Right _ ->
          pure (Right (importedCount + 1, skippedCount, Set.insert (ownerUserId, targetUsername) knownKeys))
        Left err ->
          case Ex.fromException err of
            Just sqlErr
              | isUniqueViolation sqlErr -> do
                  putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting share owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
                  pure (Right (importedCount, skippedCount + 1, Set.insert (ownerUserId, targetUsername) knownKeys))
            _ ->
              pure (Left ("Trip-sharing startup import failed while writing share owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ show err))

importSingleTripSubscription
  :: Connection
  -> Either String (Int, Int, Set.Set (String, String))
  -> (String, String)
  -> IO (Either String (Int, Int, Set.Set (String, String)))
importSingleTripSubscription _ (Left err) _ = pure (Left err)
importSingleTripSubscription conn (Right (importedCount, skippedCount, knownKeys)) (ownerUserId, targetUsername) =
  if Set.member (ownerUserId, targetUsername) knownKeys
    then do
      putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting subscription owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
      pure (Right (importedCount, skippedCount + 1, knownKeys))
    else do
      writeResult <- Ex.try
        (execute conn
          "INSERT INTO trip_subscriptions (owner_user_id, target_username) VALUES (?, ?)"
          (ownerUserId, targetUsername))
        :: IO (Either Ex.SomeException Int64)
      case writeResult of
        Right _ ->
          pure (Right (importedCount + 1, skippedCount, Set.insert (ownerUserId, targetUsername) knownKeys))
        Left err ->
          case Ex.fromException err of
            Just sqlErr
              | isUniqueViolation sqlErr -> do
                  putStrLn ("[startup][trip-sharing-import][warning] skipping conflicting subscription owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " policy=postgres-wins")
                  pure (Right (importedCount, skippedCount + 1, Set.insert (ownerUserId, targetUsername) knownKeys))
            _ ->
              pure (Left ("Trip-sharing startup import failed while writing subscription owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ show err))

loadFilesystemOwnerUserPairs :: FilePath -> String -> IO (Either String [(String, String)])
loadFilesystemOwnerUserPairs rootDir relationLabel = do
  exists <- doesDirectoryExist rootDir
  if not exists
    then do
      putStrLn ("[startup][trip-sharing-import] source " ++ relationLabel ++ " directory is missing; treating filesystem trip-sharing " ++ relationLabel ++ " source as empty")
      pure (Right [])
    else do
      entries <- listDirectory rootDir
      foldM (loadSingleOwnerUserPairs rootDir relationLabel) (Right []) (sort entries)

loadSingleOwnerUserPairs
  :: FilePath
  -> String
  -> Either String [(String, String)]
  -> FilePath
  -> IO (Either String [(String, String)])
loadSingleOwnerUserPairs _ _ (Left err) _ = pure (Left err)
loadSingleOwnerUserPairs rootDir relationLabel (Right acc) fileName
  | takeExtension fileName /= ".json" = pure (Right acc)
  | otherwise = do
      let fullPath = rootDir </> fileName
          ownerUserId = takeBaseName fileName
      content <- BL.readFile fullPath
      case decode content of
        Nothing ->
          pure (Left ("Trip-sharing startup import failed while reading filesystem " ++ relationLabel ++ " for owner_user_id=" ++ ownerUserId ++ ": invalid JSON in " ++ fullPath))
        Just usernames ->
          let normalized = sort (nub (usernames :: [String]))
              pairs = [(ownerUserId, username) | username <- normalized]
           in pure (Right (pairs ++ acc))

isUniqueViolation :: SqlError -> Bool
isUniqueViolation sqlErr = sqlState sqlErr == "23505"

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
          { dbCalendarItemKind = "legacy"
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

renderAuthBackend :: AuthBackend -> String
renderAuthBackend AuthBackendFilesystem = "filesystem"
renderAuthBackend AuthBackendPostgres = "postgres"

renderSessionBackend :: SessionBackend -> String
renderSessionBackend SessionBackendFilesystem = "filesystem"
renderSessionBackend SessionBackendPostgres = "postgres"

renderCalendarBackend :: CalendarBackend -> String
renderCalendarBackend CalendarBackendFilesystem = "filesystem"
renderCalendarBackend CalendarBackendPostgres = "postgres"

renderTripSharingBackend :: TripSharingBackend -> String
renderTripSharingBackend TripSharingBackendFilesystem = "filesystem"
renderTripSharingBackend TripSharingBackendPostgres = "postgres"

renderNoteBackend :: NoteBackend -> String
renderNoteBackend NoteBackendFilesystem = "filesystem"
renderNoteBackend NoteBackendPostgres = "postgres"

renderChecklistBackend :: ChecklistBackend -> String
renderChecklistBackend ChecklistBackendFilesystem = "filesystem"
renderChecklistBackend ChecklistBackendPostgres = "postgres"

makeCalendarRepository :: CalendarBackend -> Maybe DatabaseConfig -> IO (Either String CalendarRepository)
makeCalendarRepository CalendarBackendFilesystem _ = pure (Right defaultCalendarRepository)
makeCalendarRepository CalendarBackendPostgres mDatabaseCfg =
  case mDatabaseCfg of
    Nothing -> pure (Left "Configuration database is required when calendarBackend=postgres")
    Just dbCfg -> do
      let connectionString = renderPostgresConnectionString dbCfg
      validationResult <- verifyPostgresCalendarStorage connectionString
      case validationResult of
        Left err -> pure (Left ("Postgres calendar storage validation failed: " ++ err))
        Right () -> pure (Right (postgresCalendarRepository connectionString))

makeTripSharingRepository :: TripSharingBackend -> Maybe DatabaseConfig -> IO (Either String TripSharingRepository)
makeTripSharingRepository TripSharingBackendFilesystem _ = pure (Right defaultTripSharingRepository)
makeTripSharingRepository TripSharingBackendPostgres mDatabaseCfg =
  case mDatabaseCfg of
    Nothing -> pure (Left "Configuration database is required when tripSharingBackend=postgres")
    Just dbCfg -> do
      let connectionString = renderPostgresConnectionString dbCfg
      validationResult <- verifyPostgresTripSharingStorage connectionString
      case validationResult of
        Left err -> pure (Left ("Postgres trip-sharing storage validation failed: " ++ err))
        Right () -> pure (Right (postgresTripSharingRepository connectionString))

makeNoteRepository :: NoteBackend -> Maybe DatabaseConfig -> IO (Either String NoteRepository)
makeNoteRepository NoteBackendFilesystem _ = pure (Right defaultNoteRepository)
makeNoteRepository NoteBackendPostgres mDatabaseCfg =
  case mDatabaseCfg of
    Nothing -> pure (Left "Configuration database is required when noteBackend=postgres")
    Just dbCfg -> do
      let connectionString = renderPostgresConnectionString dbCfg
      validationResult <- verifyPostgresNoteStorage connectionString
      case validationResult of
        Left err -> pure (Left ("Postgres note storage validation failed: " ++ err))
        Right () -> pure (Right (postgresNoteRepository connectionString))

makeChecklistRepository :: ChecklistBackend -> Maybe DatabaseConfig -> IO (Either String ChecklistRepository)
makeChecklistRepository ChecklistBackendFilesystem _ = pure (Right defaultChecklistRepository)
makeChecklistRepository ChecklistBackendPostgres mDatabaseCfg =
  case mDatabaseCfg of
    Nothing -> pure (Left "Configuration database is required when checklistBackend=postgres")
    Just dbCfg -> do
      let connectionString = renderPostgresConnectionString dbCfg
      validationResult <- verifyPostgresChecklistStorage connectionString
      case validationResult of
        Left err -> pure (Left ("Postgres checklist storage validation failed: " ++ err))
        Right () -> pure (Right (postgresChecklistRepository connectionString))

makeSessionStore :: SessionBackend -> Maybe DatabaseConfig -> FilePath -> SessionConfig -> IO (Either String SessionStore)
makeSessionStore SessionBackendFilesystem _ cd sessionCfg =
  Right <$> mkFileSessionStore (cd </> "data" </> "sessions") sessionCfg
makeSessionStore SessionBackendPostgres mDatabaseCfg _ sessionCfg =
  case mDatabaseCfg of
    Nothing -> pure (Left "Configuration database is required when session.sessionBackend=postgres")
    Just dbCfg -> do
      let connectionString = renderPostgresConnectionString dbCfg
      validationResult <- verifyPostgresSessionStorage connectionString
      case validationResult of
        Left err -> pure (Left ("Postgres session storage validation failed: " ++ err))
        Right () -> pure (Right (mkSessionStore (mkPostgresSessionRepository connectionString) sessionCfg))

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

apiController :: AuthRepository -> CalendarRepository -> TripSharingRepository -> NoteRepository -> ChecklistRepository -> MVar [UTCTime] -> FilePath -> AppConfig -> SessionStore -> ServerPartT IO Response
apiController authRepo calendarRepo tripSharingRepo noteRepo checklistRepo signupRateLimitState tmpDir appConfig sessionStore =
  let sessionCfg = sessionConfig appConfig
      bootstrapAdmin = bootstrapAdminUsername appConfig
  in dir "api" $ msum [ signupController authRepo signupRateLimitState tmpDir bootstrapAdmin
                      , signinController authRepo sessionCfg sessionStore
                      , signoutController sessionCfg sessionStore
                      , requireAuth sessionCfg sessionStore (authController authRepo)
                      , requireAuth sessionCfg sessionStore (`noteController` noteRepo)
                      , requireAuth sessionCfg sessionStore (`checklistController` checklistRepo)
                      , requireAuth sessionCfg sessionStore tripPlacesController
                      , requireAuth sessionCfg sessionStore (tripSharingController authRepo tripSharingRepo calendarRepo)
                      , requireAuth sessionCfg sessionStore (agendaController calendarRepo)
                      , requireAuth sessionCfg sessionStore (adminController authRepo bootstrapAdmin)
                      ]

homePage :: ServerPartT IO Response
homePage = do
    nullDir
    cd <- liftIO getCurrentDirectory
    serveFileFrom (cd </> "static/") (guessContentTypeM mimeTypes) "index.html"

maxSignupBodyBytes :: Int64
maxSignupBodyBytes = 4096

signupBodyPolicy :: FilePath -> BodyPolicy
signupBodyPolicy tmpDir = defaultBodyPolicy tmpDir 0 maxSignupBodyBytes maxSignupBodyBytes

isTooLargeBodyError :: String -> Bool
isTooLargeBodyError err = "x-www-form-urlencoded content longer than BodyPolicy.maxRAM=" `isPrefixOf` err

signupController :: AuthRepository -> MVar [UTCTime] -> FilePath -> String -> ServerPartT IO Response
signupController authRepo signupRateLimitState tmpDir bootstrapAdmin = dir "signup" $ do
    nullDir
    method POST
    rq <- askRq
    (_, mBodyErr) <- liftIO $ bodyInput (signupBodyPolicy tmpDir) rq
    case mBodyErr of
      Just bodyErr | isTooLargeBodyError bodyErr -> requestEntityTooLarge $ jsonMessage "Body too large"
      Just _ -> badRequest "Unable to decode request body"
      Nothing -> do
        log "Reading signup body"
        body <- askRq >>= takeRequestBody
        maybe (badRequest "Empty body")
              handleBody
              body
    where handleBody :: RqBody -> ServerPartT IO Response --AppM Response
          handleBody body =
            maybe (badRequest "Unable to decode the body as a SignupData")
                  doCreateUser
                  (decode $ unBody body)

          doCreateUser :: AuthRequest -> ServerPartT IO Response --AppM Response
          doCreateUser signupRequest = do
            allowed <- liftIO $ allowSignupRequest signupRateLimitState
            if not allowed
              then tooManyRequests "Too many signup attempts. Please retry later."
              else do
                res <- liftIO $ runExceptT $ createUserWithBootstrapAdmin authRepo (Just bootstrapAdmin) signupRequest
                either toServerResponse
                       (const $ ok emptyResponse)
                       res

signinController :: AuthRepository -> SessionConfig -> SessionStore -> ServerPartT IO Response
signinController authRepo sessionConfig sessionStore = dir "signin" $ do
  nullDir
  method POST
  withBusinessHandlingAndInput (signinUser authRepo) $ \profile -> do
    sid <- liftIO $ createSessionForUser sessionStore (authProfileUsername profile)
    let cookieValue = signSessionId (sessionSecret sessionConfig) sid
    addCookie Session (buildSessionCookie sessionConfig cookieValue)
    ok (jsonResponse profile)

signoutController :: SessionConfig -> SessionStore -> ServerPartT IO Response
signoutController sessionConfig sessionStore = dir "signout" $ do
  nullDir
  method POST
  revokeAll <- isSignoutAllRequested
  mToken <- getSessionCookieValue sessionConfig
  case mToken >>= verifyAndExtractSessionId (sessionSecret sessionConfig) of
    Nothing -> unauthorized $ jsonMessage "Not authenticated"
    Just sid -> do
      _ <- liftIO $ if revokeAll then revokeAllForSession sessionStore sid else revokeSession sessionStore sid
      addCookie Expired (buildSessionCookie sessionConfig "")
      ok emptyResponse

authController :: AuthRepository -> AppContext -> ServerPartT IO Response
authController authRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "auth" $
    dir "profile" $ do
      nullDir
      method GET
      profileResult <- liftIO $ runExceptT $ loadAuthenticatedProfile authRepo principalUserId
      either toServerResponse
             (ok . jsonResponse)
             profileResult



isSignoutAllRequested :: ServerPartT IO Bool
isSignoutAllRequested = do
  mRaw <- (Just <$> look "all") `mplus` pure Nothing
  pure $ case fmap (map toLower) mRaw of
    Just "true" -> True
    Just "1" -> True
    Just "false" -> False
    Just "0" -> False
    _ -> False

tooManyRequests :: FilterMonad Response m => String -> m Response
tooManyRequests = HServer.badRequest . toResponse

allowSignupRequest :: MVar [UTCTime] -> IO Bool
allowSignupRequest state = do
  now <- getCurrentTime
  let windowStart = addUTCTime (-60) now
      maxRequests = 5
  modifyMVar state $ \timestamps -> do
    let recent = filter (> windowStart) timestamps
    if length recent >= maxRequests
      then pure (recent, False)
      else pure (now : recent, True)

withBusinessHandling :: (FromJSON a, ToServerResponse e) => (a -> ExceptT e IO r) -> ServerPartT IO Response
withBusinessHandling handle = do
    body <- askRq >>= takeRequestBody
    maybe (badRequest "Empty body")
          handleBody
          body
    where handleBody :: RqBody -> ServerPartT IO Response --AppM Response
          handleBody body = maybe (badRequest "Unable to decode the body as a SignupData")
                  (processDecodedBody handle)
                                  (decode' $ unBody body)

          processDecodedBody :: ToServerResponse e => (a -> ExceptT e IO r) -> a -> ServerPartT IO Response
          processDecodedBody handle input = do
            res <- liftIO $ runExceptT $ handle input
            either toServerResponse
                   (const $ ok emptyResponse)
                   res


withBusinessHandlingAndInput :: (FromJSON a, ToServerResponse e) => (a -> ExceptT e IO r) -> (r -> ServerPartT IO Response) -> ServerPartT IO Response
withBusinessHandlingAndInput handle onSuccess = do
    body <- askRq >>= takeRequestBody
    maybe (badRequest "Empty body")
          handleBody
          body
    where
      handleBody :: RqBody -> ServerPartT IO Response
      handleBody body = maybe (badRequest "Unable to decode the body as a SignupData")
                             process
                             (decode' $ unBody body)
      process input = do
        res <- liftIO $ runExceptT $ handle input
        either toServerResponse
               onSuccess
               res

buildSessionCookie :: SessionConfig -> String -> Cookie
buildSessionCookie sessionConfig cookieValue =
  (mkCookie (sessionCookieName sessionConfig) cookieValue)
    { secure = sessionCookieSecure sessionConfig
    , httpOnly = True
    , sameSite = SameSiteLax
    }

getSessionCookieValue :: SessionConfig -> ServerPartT IO (Maybe String)
getSessionCookieValue sessionConfig = do
  mCookieHeader <- getHeaderM "cookie"
  pure $ mCookieHeader >>= extractCookie (sessionCookieName sessionConfig) . unpack

extractCookie :: String -> String -> Maybe String
extractCookie cookieName rawCookieHeader =
  let chunks = splitOn ';' rawCookieHeader
      normalized = map trim chunks
      targetPrefix = cookieName ++ "="
      matches = filter (isPrefixOf targetPrefix) normalized
  in case matches of
       [] -> Nothing
       (x:_) -> Just $ unquoteCookieValue $ drop (length targetPrefix) x

unquoteCookieValue :: String -> String
unquoteCookieValue value =
  case value of
    ('"':rest) | not (null rest) && last rest == '"' -> init rest
    _ -> value

splitOn :: Char -> String -> [String]
splitOn sep s =
  case break (== sep) s of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn sep after

trim :: String -> String
trim = dropWhile (== ' ')

requireAuth :: SessionConfig -> SessionStore -> (AppContext -> ServerPartT IO Response) -> ServerPartT IO Response
requireAuth sessionConfig sessionStore handler = do
  mToken <- getSessionCookieValue sessionConfig
  case mToken >>= verifyAndExtractSessionId (sessionSecret sessionConfig) of
    Nothing -> unauthorized $ jsonMessage "Not authenticated"
    Just sid -> do
      mPrincipal <- liftIO $ resolveSession sessionStore sid
      case mPrincipal of
        Nothing -> unauthorized $ jsonMessage "Not authenticated"
        Just principal -> handler AppContext { sessionPrincipal = principal }

requireApprovedAdmin :: AuthRepository -> AppContext -> ServerPartT IO Response -> ServerPartT IO Response
requireApprovedAdmin authRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } handler = do
  adminCheck <- liftIO $ isApprovedAdmin authRepo principalUserId
  case adminCheck of
    Left _ -> internalServerError authInternalError
    Right False -> HServer.forbidden $ jsonMessage "Admin privileges required"
    Right True -> handler


noteController :: AppContext -> NoteRepository -> ServerPartT IO Response
noteController _ noteRepo = dir "note" (notesChecklistHandlers "note" noteRepo)

checklistController :: AppContext -> ChecklistRepository -> ServerPartT IO Response
checklistController _ checklistRepo = dir "checklist" (notesChecklistHandlers "checklist" checklistRepo)

notesChecklistHandlers :: Content a => String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistHandlers crudTypeName repo =
  msum
    [ notesChecklistGet crudTypeName repo
    , notesChecklistPost crudTypeName repo
    , notesChecklistDelete crudTypeName repo
    , notesChecklistPut crudTypeName repo
    ]

notesChecklistGet :: Content a => String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistGet crudTypeName repo = do
  nullDir
  method GET
  log ("crud GET on " ++ crudTypeName)
  recover
    (\err -> genericInternalError ("Unexpected problem during retrieving all " ++ crudTypeName ++ "s:\n\t" ++ show err))
    (ok . jsonResponse)
    (repoListItems repo)

notesChecklistPost :: Content a => String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistPost crudTypeName repo = do
  nullDir
  method POST
  log ("crud POST on " ++ crudTypeName)
  body <- askRq >>= takeRequestBody
  let
    handleBody :: RqBody -> ServerPartT IO Response
    handleBody rqBody = do
      let bodyBS = unBody rqBody
          content = decode bodyBS
      log ("Getting body bytestrings: " ++ show bodyBS)
      log ("Getting deserialized content: " ++ show content)
      fmap (createNotesChecklistContent crudTypeName repo) content `orElse` genericInternalError "Unexpected problem during note creation"
  fmap handleBody body `orElse` ok emptyResponse

createNotesChecklistContent :: String -> NotesChecklistRepository a -> a -> ServerPartT IO Response
createNotesChecklistContent crudTypeName repo content = do
  recover (logThenGenericInternalErrorName crudTypeName) (ok . jsonResponse) $ repoCreateItem repo content

notesChecklistDelete :: String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistDelete crudTypeName repo = do
  method DELETE
  log ("crud DELETE on " ++ crudTypeName)
  path $ \pathId -> do
    nullDir
    recover (handleDeletionError pathId) (\() -> ok emptyResponse) $ repoDeleteItemById repo pathId

notesChecklistPut :: Content a => String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistPut crudTypeName repo = do
  nullDir
  method PUT
  log ("crud PUT on " ++ crudTypeName)
  body <- askRq >>= takeRequestBody
  let
    handleBody :: RqBody -> ServerPartT IO Response
    handleBody rqBody = do
      let bodyBS = unBody rqBody
          update = decode bodyBS
      log ("Getting body bytestrings: " ++ show bodyBS)
      log ("Getting deserialized content: " ++ show update)
      fmap (handleUpdateByRepository repo) update `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
  fmap handleBody body `orElse` ok emptyResponse

handleUpdateByRepository :: Content a => NotesChecklistRepository a -> Identifiable a -> ServerPartT IO Response
handleUpdateByRepository repo update =
  recoverWith (const . notFound $ jsonMessage "Unable to find storage dir")
              (ok . jsonResponse <$> repoUpdateItem repo update)

tripPlacesController :: AppContext -> ServerPartT IO Response
tripPlacesController _ = dir "v1" $ dir "trip-places" $ do
  nullDir
  method GET
  ok (jsonResponse tripPlacesCatalog)

adminController :: AuthRepository -> String -> AppContext -> ServerPartT IO Response
adminController authRepo bootstrapAdminUsername appContext@AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "v1" $ dir "admin" $
    requireApprovedAdmin authRepo appContext $
      msum [ dir "pending-signups" $
               msum [ pendingSignupsList
                    , pendingSignupApprove
                    , pendingSignupDelete
                    ]
           , dir "users" $
               msum [ approvedUsersList
                    , approvedUserDelete
                    ]
           ]
  where
    pendingSignupsList = do
      nullDir
      method GET
      pendingUsersResult <- liftIO $ listPendingUsers authRepo
      case pendingUsersResult of
        Left _ -> internalServerError authInternalError
        Right pendingUsers -> ok (jsonResponse (map PendingSignupApproval pendingUsers))

    pendingSignupApprove = do
      dir "approve" $ do
        nullDir
        method POST
        body <- askRq >>= takeRequestBody
        maybe (badRequest "Empty body") handleBody body

    pendingSignupDelete = do
      path $ \username -> do
        nullDir
        method DELETE
        result <- liftIO $ runExceptT $ deletePendingUser authRepo username
        either toServerResponse
               (const $ ok emptyResponse)
               result

    approvedUsersList = do
      nullDir
      method GET
      approvedUsersResult <- liftIO $ listApprovedUsers authRepo
      case approvedUsersResult of
        Left _ -> internalServerError authInternalError
        Right approvedUsers -> ok (jsonResponse approvedUsers)

    approvedUserDelete = do
      path $ \username -> do
        nullDir
        method DELETE
        result <- liftIO $ runExceptT $ deleteApprovedUser authRepo bootstrapAdminUsername principalUserId username
        either toServerResponse
               (const $ ok emptyResponse)
               result

    handleBody :: RqBody -> ServerPartT IO Response
    handleBody rqBody =
      case decode' (unBody rqBody) :: Maybe PendingSignupApproval of
        Nothing -> badRequest "Unable to decode the body as a PendingSignupApproval"
        Just (PendingSignupApproval username)
          | null username -> badRequest "username is required"
          | otherwise -> do
              result <- liftIO $ runExceptT $ approveUser authRepo username
              either toServerResponse
                     (const $ ok emptyResponse)
                     result

tripSharingController :: AuthRepository -> TripSharingRepository -> CalendarRepository -> AppContext -> ServerPartT IO Response
tripSharingController authRepo tripSharingRepo calendarRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "v1" $ dir "trip-sharing" $ msum [ dir "shares" $ msum [ sharesList
                                                             , sharesAdd
                                                             , sharesDelete
                                                             ]
                                      , dir "subscriptions" $ msum [ subscriptionsList
                                                                   , subscriptionsAdd
                                                                   , subscriptionsDelete
                                                                   ]
                                      , dir "period-trips" periodTripsList
                                      ]
  where
    periodTripsList = do
      nullDir
      method GET
      mStart <- (Just <$> look "start") `mplus` pure Nothing
      mEnd <- (Just <$> look "end") `mplus` pure Nothing
      case parsePeriodTripBounds mStart mEnd of
        Left message -> badRequest message
        Right (periodStart, periodEnd) -> do
          visibleUsersResult <- liftIO $ resolveVisiblePeriodTripUsers tripSharingRepo principalUserId
          case visibleUsersResult of
            Left () -> internalServerError emptyResponse
            Right visibleUsers -> do
              groupsResult <- liftIO $ loadPeriodTripsForUsers calendarRepo visibleUsers periodStart periodEnd
              case groupsResult of
                Left () -> internalServerError emptyResponse
                Right groups -> ok (jsonResponse groups)

    sharesList = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListSharedUsers tripSharingRepo principalUserId)
      case result of
        Left _ -> internalServerError emptyResponse
        Right usernames -> ok (jsonResponse (map TripSharingUser usernames))

    sharesAdd = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") handleBody body
      where
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody =
          case decode' (unBody rqBody) :: Maybe TripSharingUser of
            Nothing -> badRequest "Unable to decode the body as a TripSharingUser"
            Just (TripSharingUser username)
              | null username -> badRequest "username is required"
              | username == principalUserId -> badRequest "username must not be the authenticated user"
              | otherwise -> do
                  exists <- liftIO $ userExists authRepo username
                  if not exists
                    then badRequest "username must reference an existing user"
                    else do
                      result <- liftIO $ runExceptT (repoAddSharedUser tripSharingRepo principalUserId username)
                      case result of
                        Left _ -> internalServerError emptyResponse
                        Right () -> ok emptyResponse

    sharesDelete = do
      method DELETE
      path $ \username -> do
        nullDir
        result <- liftIO $ runExceptT (repoDeleteSharedUser tripSharingRepo principalUserId username)
        case result of
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

    subscriptionsList = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListSubscribedUsers tripSharingRepo principalUserId)
      case result of
        Left _ -> internalServerError emptyResponse
        Right usernames -> ok (jsonResponse (map TripSharingUser usernames))

    subscriptionsAdd = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") handleBody body
      where
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody =
          case decode' (unBody rqBody) :: Maybe TripSharingUser of
            Nothing -> badRequest "Unable to decode the body as a TripSharingUser"
            Just (TripSharingUser username)
              | null username -> badRequest "username is required"
              | username == principalUserId -> badRequest "username must not be the authenticated user"
              | otherwise -> do
                  exists <- liftIO $ userExists authRepo username
                  if not exists
                    then badRequest "username must reference an existing user"
                    else do
                      result <- liftIO $ runExceptT (repoAddSubscribedUser tripSharingRepo principalUserId username)
                      case result of
                        Left _ -> internalServerError emptyResponse
                        Right () -> ok emptyResponse

    subscriptionsDelete = do
      method DELETE
      path $ \username -> do
        nullDir
        result <- liftIO $ runExceptT (repoDeleteSubscribedUser tripSharingRepo principalUserId username)
        case result of
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

agendaController :: CalendarRepository -> AppContext -> ServerPartT IO Response
agendaController calendarRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "v1" $ dir "calendar-items" $ msum [ agendaList
                                         , agendaCreate
                                         , agendaDelete
                                         ]
  where
    agendaList = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListCalendarItemsForUser calendarRepo principalUserId)
      case result of
        Left _ -> internalServerError emptyResponse
        Right items -> ok (jsonResponse items)

    agendaCreate = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body")
            handleBody
            body
      where
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody =
          case decode' (unBody rqBody) :: Maybe Agenda.CalendarItem of
            Just (Agenda.NewCalendarItem {Agenda.content}) -> do
              validation <- liftIO $ validateTripWrite calendarRepo principalUserId Nothing content
              case validation of
                TripWriteValid -> do
                  result <- liftIO $ runExceptT (repoCreateCalendarItem calendarRepo principalUserId content)
                  case result of
                    Left _ -> internalServerError emptyResponse
                    Right created -> ok (jsonResponse created)
                TripWriteBadRequest message -> badRequest message
                TripWriteNotFound -> notFound emptyResponse
                TripWriteTechnicalFailure -> internalServerError emptyResponse
            Just (Agenda.ServerCalendarItem {Agenda.content, Agenda.itemId}) -> do
              validation <- liftIO $ validateTripWrite calendarRepo principalUserId (Just itemId) content
              case validation of
                TripWriteValid -> do
                  result <- liftIO $ runExceptT (repoUpdateCalendarItem calendarRepo principalUserId itemId content)
                  case result of
                    Left NotFound -> notFound emptyResponse
                    Left _ -> internalServerError emptyResponse
                    Right updated -> ok (jsonResponse updated)
                TripWriteBadRequest message -> badRequest message
                TripWriteNotFound -> notFound emptyResponse
                TripWriteTechnicalFailure -> internalServerError emptyResponse
            Nothing ->
              case decode' (unBody rqBody) :: Maybe Agenda.ValidateRequest of
                Nothing -> badRequest "Unable to decode the body as a CalendarItem or ValidateRequest"
                Just (Agenda.ValidateRequest itemId minutes) -> do
                  result <- liftIO $ runExceptT (repoUpdateCalendarItemDuration calendarRepo principalUserId itemId minutes)
                  case result of
                    Left NotFound -> notFound emptyResponse
                    Left _ -> internalServerError emptyResponse
                    Right _ -> ok emptyResponse

    agendaDelete = do
      method DELETE
      path $ \itemId -> do
        nullDir
        result <- liftIO $ runExceptT (repoDeleteCalendarItemById calendarRepo principalUserId itemId)
        case result of
          Left NotFound -> notFound emptyResponse
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

crudGet ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudGet crudConfig = do
    nullDir
    method GET
    log ("crud GET on " ++ crudTypeDenomination crudConfig)
    recover (\err -> genericInternalError $ "Unexpected problem during retrieving all " ++ crudTypeDenomination crudConfig ++ "s:\n\t" ++ show err) (successResponse . handlePotentialParsingErrors) $ getItems crudConfig

successResponse :: ToJSON a => IO a -> ServerPartT IO Response
successResponse action = do
    a <- liftIO action
    (ok . jsonResponse) a

handlePotentialParsingErrors :: [ExceptT CrudReadException IO (Identifiable a)] -> IO [Identifiable a]
handlePotentialParsingErrors = foldM accumulateSuccessOrLogError []

accumulateSuccessOrLogError :: [Identifiable a] -> ExceptT CrudReadException IO (Identifiable a) -> IO [Identifiable a]
accumulateSuccessOrLogError acc parsingResult = do
    parsingTry <- runExceptT parsingResult
    case parsingTry of
        Left e -> do
            log ("Unexpected parsing exception: " ++ show e)
            return acc
        Right succ -> return (succ:acc)

crudPost ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPost crudConfig = do
    nullDir
    method POST
    log ("crud POST on " ++ crudTypeDenomination crudConfig)
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let bodyBS = unBody rqBody
                noteContent = decode bodyBS :: Content a => Maybe a
            log ("Getting body bytestrings: " ++ show bodyBS)
            log ("Getting deserialized content: " ++ show noteContent)
            fmap (createNoteContent crudConfig) noteContent `orElse` genericInternalError "Unexpected problem during note creation"
    fmap handleBody body `orElse` ok emptyResponse

createNoteContent :: CRUDEngine crudType a => crudType -> a -> ServerPartT IO Response
createNoteContent crudConfig noteContent = do
    recover (logThenGenericInternalError crudConfig) (ok . jsonResponse) $ createItem crudConfig noteContent

logThenGenericInternalError :: (Show e, CRUDEngine crudType a) => crudType -> e -> ServerPartT IO Response
logThenGenericInternalError crudConfig e = do
    log ("Unexpected error during creation of " ++ crudTypeDenomination crudConfig ++ ": " ++ show e)
    emptyInternalError

logThenGenericInternalErrorName :: Show e => String -> e -> ServerPartT IO Response
logThenGenericInternalErrorName crudTypeName e = do
    log ("Unexpected error during creation of " ++ crudTypeName ++ ": " ++ show e)
    emptyInternalError

crudDelete :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudDelete crudConfig = do
    method DELETE
    log ("crud DELETE on " ++ crudTypeDenomination crudConfig)
    path (\pathId -> do
        nullDir
        recover (handleDeletionError pathId) (\() -> ok emptyResponse) $ deleteItem crudConfig pathId)

handleDeletionError :: String -> CrudWriteException -> ServerPartT IO Response
handleDeletionError pathId err = do
    logDeletionError pathId err
    notFound emptyResponse

logDeletionError pathId s = log ("Error while deleting item " ++ pathId ++ ": " ++ show s)

crudPut :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPut crudConfig = do
    nullDir
    method PUT
    log ("crud PUT on " ++ crudTypeDenomination crudConfig)
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let bodyBS = unBody rqBody
            let noteUpdate = decode bodyBS
            log ("Getting body bytestrings: " ++ show bodyBS)
            log ("Getting deserialized content: " ++ show noteUpdate)
            fmap (handleUpdate crudConfig) noteUpdate `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
    fmap handleBody body `orElse` ok emptyResponse -- do not send back ok when there is no body

handleUpdate :: CRUDEngine crudType a => crudType -> Identifiable a -> ServerPartT IO Response
handleUpdate crudConfig update =
    recoverWith (const . notFound $ jsonMessage "Unable to find storage dir")
                (ok.jsonResponse <$> modifyItem crudConfig update)

serveStaticResource :: ServerPartT IO Response
serveStaticResource = do
    method GET
    log "Serving static resource"
    dir "static" $ uriRest (\rest -> do
        cd <- liftIO getCurrentDirectory -- replace with configuration data directory
        case rest of
          [] -> badRequest "toto"
          [a] -> badRequest "toto"
          (_:withoutFrontSlash) -> serveFileFrom (cd </> "static/") (guessContentTypeM mimeTypes) withoutFrontSlash) -- check serveFileFrom for filesystem attacks with ..

orElse :: Maybe a -> a -> a
(Just a) `orElse` _ = a
_        `orElse` b = b

recoverIO :: (MonadIO m, Monad m) => ExceptT e IO (m a) -> (e -> m a) -> m a
recoverIO exceptT f = join $ liftIO $ fmap (either f id) (runExceptT exceptT)

recoverWith :: (MonadIO m, Monad m) => (e -> m a) -> ExceptT e IO (m a) -> m a
recoverWith = flip recoverIO

orElseIO :: (MonadIO m, Monad m) => MaybeT IO (m a) -> m a -> m a
orElseIO maybe alt = do
    tmp <- liftIO $ runMaybeT maybe
    tmp `orElse` alt

withDefaultIO :: (MonadIO m, Monad m) => m a -> MaybeT IO (m a) -> m a
withDefaultIO = flip orElseIO

recover :: (e -> ServerPartT IO Response) -> (b -> ServerPartT IO Response) -> ExceptT e IO b -> ServerPartT IO Response
recover errorHandler successHandler errorMonad = do
    errorOrNot <- lift $ runExceptT errorMonad
    either errorHandler successHandler errorOrNot

genericInternalError :: String -> ServerPartT IO Response
genericInternalError s = do
    log ("Internal error: \n\t" ++ s)
    emptyInternalError

emptyInternalError :: ServerPartT IO Response
emptyInternalError = internalServerError emptyResponse

log :: (Show s, MonadIO m) => s -> m ()
log s = do
  liftIO $ print s >> hFlush stdout

infixr 4 <%>

(<%>) :: Functor f => a -> f (a -> b) -> f b
a <%> f = (a &) <$> f
