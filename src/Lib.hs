{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( runApp
    ) where

import Prelude hiding (log, writeFile)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), decode, encode, decode', eitherDecodeFileStrict', (.:), (.:?), (.=), withObject, object)
import Data.Function ((&))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe, mapMaybe, catMaybes)
import Data.Int (Int64)
import Control.Monad (msum, mzero, join, foldM, when, mplus)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Either (either)
import Data.ByteString.Char8 (unpack)
import Data.List (isPrefixOf, sortOn)
import Data.Char (toLower)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Happstack.Server (FilterMonad, Response, ServerPartT, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse, method, ok, internalServerError, notFound, dir, Method(GET, POST, DELETE, PUT), Conf(..), addCookie, mkCookie, CookieLife(Session, Expired), getHeaderM, unauthorized, requestEntityTooLarge, look, setResponseCode)
import qualified Happstack.Server as HServer
import Happstack.Server.Internal.Cookie (Cookie(..), SameSite(..))
import Happstack.Server.Internal.MessageWrap (bodyInput, BodyPolicy)
import Model (NoteContent, ChecklistContent, Content, Identifiable(..))
import qualified AgendaModel as Agenda
import AgendaStorage (createCalendarItem, defaultCalendarStorageConfig, deleteCalendarItem, getCalendarItems, updateCalendarItem, updateCalendarItemDuration, CalendarStorageError(..))
import TripSharingStorage
  ( addSharedUser
  , addSubscribedUser
  , defaultTripShareStorageConfig
  , defaultTripSubscriptionStorageConfig
  , deleteSharedUser
  , deleteSubscribedUser
  , getSharedUsers
  , getSubscribedUsers
  )
import CrudStorage (createItem, getAllItems, deleteItem, modifyItem)
import Crud
import NoteCrud (NoteServiceConfig(..), defaultNoteServiceConfig)
import ChecklistCrud (ChecklistServiceConfig(..), defaultChecklistServiceConfig)
import System.Directory (doesFileExist, getCurrentDirectory, canonicalizePath, getTemporaryDirectory)
import System.FilePath ((</>), pathSeparator)
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
import Auth (AuthRequest(..), AuthRequestError(..), AuthError(..), AuthenticatedProfile(..), createUserWithBootstrapAdmin, loadAuthenticatedProfile, signinUser, userExists, isApprovedAdmin, listPendingUsers, listApprovedUsers, approveUser, deletePendingUser, deleteApprovedUser)
import Session (SessionConfig(..), SessionPrincipal(..), SessionStore(..), defaultSessionConfig, mkFileSessionStore, signSessionId, verifyAndExtractSessionId)

type AppM a = ExceptT String (ServerPartT IO) a


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
  { appSession :: SessionConfigFile
  , appAuth :: AuthConfigFile
  } deriving (Generic)

instance FromJSON AppConfigFile where
  parseJSON = withObject "AppConfigFile" $ \v -> AppConfigFile
    <$> v .: "session"
    <*> v .: "auth"

data AppConfig = AppConfig
  { sessionConfig :: SessionConfig
  , bootstrapAdminUsername :: String
  }

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

validateTripWrite :: String -> Maybe String -> Agenda.CalendarItemContent -> IO TripWriteValidation
validateTripWrite principalUserId mCurrentItemId content =
  case content of
    Agenda.TripCalendarItemContent tripContent -> do
      existingItems <- getCalendarItems defaultCalendarStorageConfig principalUserId
      pure (validateTripContent tripPlaceNames existingItems mCurrentItemId tripContent)
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

resolveVisiblePeriodTripUsers :: String -> IO (Either () [String])
resolveVisiblePeriodTripUsers principalUserId = do
  subscribedUsersResult <- getSubscribedUsers defaultTripSubscriptionStorageConfig principalUserId
  case subscribedUsersResult of
    Left _ -> pure (Left ())
    Right subscribedUsers -> do
      visibilityResults <- mapM isVisibleToPrincipal subscribedUsers
      pure $ case sequence visibilityResults of
        Left _ -> Left ()
        Right visibleUsers -> Right [username | (username, True) <- visibleUsers]
  where
    isVisibleToPrincipal username = do
      sharedUsersResult <- getSharedUsers defaultTripShareStorageConfig username
      pure $ case sharedUsersResult of
        Left _ -> Left ()
        Right sharedUsers -> Right (username, principalUserId `elem` sharedUsers)

loadPeriodTripsForUsers :: [String] -> LocalTime -> LocalTime -> IO (Either () [PeriodTripsUser])
loadPeriodTripsForUsers usernames periodStart periodEnd = do
  groups <- mapM buildUserGroup usernames
  pure $ fmap catMaybes (sequence groups)
  where
    buildUserGroup username = do
      items <- getCalendarItems defaultCalendarStorageConfig username
      pure $ case selectPeriodTrips periodStart periodEnd items of
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
toAppConfig secret AppConfigFile {appSession = SessionConfigFile {sessionCookieNameFile, sessionAbsoluteTtlSecondsFile, sessionIdleTtlSecondsFile}, appAuth = AuthConfigFile {bootstrapAdminUsernameFile}} mCookieSecure
  | null bootstrapAdminUsernameFile = Left "Configuration auth.bootstrapAdminUsername cannot be empty"
  | otherwise = Right AppConfig
      { sessionConfig =
          defaultSessionConfig
            { sessionSecret = secret
            , sessionCookieName = fromMaybe (sessionCookieName defaultSessionConfig) sessionCookieNameFile
            , sessionAbsoluteTtlSeconds = fromIntegral (fromMaybe (round (sessionAbsoluteTtlSeconds defaultSessionConfig)) sessionAbsoluteTtlSecondsFile)
            , sessionIdleTtlSeconds = fromIntegral (fromMaybe (round (sessionIdleTtlSeconds defaultSessionConfig)) sessionIdleTtlSecondsFile)
            , sessionCookieSecure = fromMaybe (sessionCookieSecure defaultSessionConfig) mCookieSecure
            }
      , bootstrapAdminUsername = bootstrapAdminUsernameFile
      }

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
        sessionStore <- mkFileSessionStore (cd </> "data" </> "sessions") sessionCfg
        simpleHTTP nullConf { port = 8081 } $ do
            log "Incoming request" >> log "=========================END REQUEST====================\n"
            msum [ homePage
                 , apiController signupRateLimitState tmpDir appConfig sessionStore
                 , serveStaticResource
                 , mzero
                 ]

apiController :: MVar [UTCTime] -> FilePath -> AppConfig -> SessionStore -> ServerPartT IO Response
apiController signupRateLimitState tmpDir appConfig sessionStore =
  let sessionCfg = sessionConfig appConfig
      bootstrapAdmin = bootstrapAdminUsername appConfig
  in dir "api" $ msum [ signupController signupRateLimitState tmpDir bootstrapAdmin
                      , signinController sessionCfg sessionStore
                      , signoutController sessionCfg sessionStore
                      , requireAuth sessionCfg sessionStore authController
                      , requireAuth sessionCfg sessionStore noteController
                      , requireAuth sessionCfg sessionStore checklistController
                      , requireAuth sessionCfg sessionStore tripPlacesController
                      , requireAuth sessionCfg sessionStore tripSharingController
                      , requireAuth sessionCfg sessionStore agendaController
                      , requireAuth sessionCfg sessionStore (adminController bootstrapAdmin)
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

signupController :: MVar [UTCTime] -> FilePath -> String -> ServerPartT IO Response
signupController signupRateLimitState tmpDir bootstrapAdmin = dir "signup" $ do
    nullDir
    method POST
    rq <- askRq
    (_, mBodyErr) <- liftIO $ bodyInput (signupBodyPolicy tmpDir) rq
    case mBodyErr of
      Just bodyErr | isTooLargeBodyError bodyErr -> requestEntityTooLarge $ jsonMessage "Body too large"
      Just _ -> badRequest "Unable to decode request body"
      Nothing -> do
        allowed <- liftIO $ allowSignupRequest signupRateLimitState
        if not allowed
          then tooManyRequests "Too many signup attempts. Please retry later."
          else do
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
            res <- liftIO $ runExceptT $ createUserWithBootstrapAdmin (Just bootstrapAdmin) signupRequest
            either toServerResponse
                   (const $ ok emptyResponse)
                   res

signinController :: SessionConfig -> SessionStore -> ServerPartT IO Response
signinController sessionConfig sessionStore = dir "signin" $ do
  nullDir
  method POST
  withBusinessHandlingAndInput signinUser $ \profile -> do
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

authController :: AppContext -> ServerPartT IO Response
authController AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "auth" $
    dir "profile" $ do
      nullDir
      method GET
      profileResult <- liftIO $ runExceptT $ loadAuthenticatedProfile principalUserId
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

requireApprovedAdmin :: AppContext -> ServerPartT IO Response -> ServerPartT IO Response
requireApprovedAdmin AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } handler = do
  adminCheck <- liftIO $ isApprovedAdmin principalUserId
  case adminCheck of
    Left _ -> internalServerError emptyResponse
    Right False -> HServer.forbidden $ jsonMessage "Admin privileges required"
    Right True -> handler


noteController :: AppContext -> ServerPartT IO Response
noteController _ = dir "note" noteHandlers
    where
        noteHandlers = msum $ defaultNoteServiceConfig <%> [ crudGet
                                                           , crudPost
                                                           , crudDelete
                                                           , crudPut
                                                           ]
checklistController :: AppContext -> ServerPartT IO Response
checklistController _ = dir "checklist" $
  msum $ defaultChecklistServiceConfig <%> [ crudGet
                                           , crudPost
                                           , crudDelete
                                           , crudPut
                                           ]

tripPlacesController :: AppContext -> ServerPartT IO Response
tripPlacesController _ = dir "v1" $ dir "trip-places" $ do
  nullDir
  method GET
  ok (jsonResponse tripPlacesCatalog)

adminController :: String -> AppContext -> ServerPartT IO Response
adminController bootstrapAdminUsername appContext@AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "v1" $ dir "admin" $
    requireApprovedAdmin appContext $
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
      pendingUsersResult <- liftIO listPendingUsers
      case pendingUsersResult of
        Left _ -> internalServerError emptyResponse
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
        result <- liftIO $ runExceptT $ deletePendingUser username
        either toServerResponse
               (const $ ok emptyResponse)
               result

    approvedUsersList = do
      nullDir
      method GET
      approvedUsersResult <- liftIO listApprovedUsers
      case approvedUsersResult of
        Left _ -> internalServerError emptyResponse
        Right approvedUsers -> ok (jsonResponse approvedUsers)

    approvedUserDelete = do
      path $ \username -> do
        nullDir
        method DELETE
        result <- liftIO $ runExceptT $ deleteApprovedUser bootstrapAdminUsername principalUserId username
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
              result <- liftIO $ runExceptT $ approveUser username
              either toServerResponse
                     (const $ ok emptyResponse)
                     result

tripSharingController :: AppContext -> ServerPartT IO Response
tripSharingController AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
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
          visibleUsersResult <- liftIO $ resolveVisiblePeriodTripUsers principalUserId
          case visibleUsersResult of
            Left () -> internalServerError emptyResponse
            Right visibleUsers -> do
              groupsResult <- liftIO $ loadPeriodTripsForUsers visibleUsers periodStart periodEnd
              case groupsResult of
                Left () -> internalServerError emptyResponse
                Right groups -> ok (jsonResponse groups)

    sharesList = do
      nullDir
      method GET
      result <- liftIO $ getSharedUsers defaultTripShareStorageConfig principalUserId
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
                  exists <- liftIO $ userExists username
                  if not exists
                    then badRequest "username must reference an existing user"
                    else do
                      result <- liftIO $ addSharedUser defaultTripShareStorageConfig principalUserId username
                      case result of
                        Left _ -> internalServerError emptyResponse
                        Right () -> ok emptyResponse

    sharesDelete = do
      method DELETE
      path $ \username -> do
        nullDir
        result <- liftIO $ deleteSharedUser defaultTripShareStorageConfig principalUserId username
        case result of
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

    subscriptionsList = do
      nullDir
      method GET
      result <- liftIO $ getSubscribedUsers defaultTripSubscriptionStorageConfig principalUserId
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
                  exists <- liftIO $ userExists username
                  if not exists
                    then badRequest "username must reference an existing user"
                    else do
                      result <- liftIO $ addSubscribedUser defaultTripSubscriptionStorageConfig principalUserId username
                      case result of
                        Left _ -> internalServerError emptyResponse
                        Right () -> ok emptyResponse

    subscriptionsDelete = do
      method DELETE
      path $ \username -> do
        nullDir
        result <- liftIO $ deleteSubscribedUser defaultTripSubscriptionStorageConfig principalUserId username
        case result of
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

agendaController :: AppContext -> ServerPartT IO Response
agendaController AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "v1" $ dir "calendar-items" $ msum [ agendaList
                                         , agendaCreate
                                         , agendaDelete
                                         ]
  where
    agendaList = do
      nullDir
      method GET
      items <- liftIO $ getCalendarItems defaultCalendarStorageConfig principalUserId
      ok (jsonResponse items)

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
              validation <- liftIO $ validateTripWrite principalUserId Nothing content
              case validation of
                TripWriteValid -> do
                  created <- liftIO $ createCalendarItem defaultCalendarStorageConfig principalUserId content
                  ok (jsonResponse created)
                TripWriteBadRequest message -> badRequest message
                TripWriteNotFound -> notFound emptyResponse
                TripWriteTechnicalFailure -> internalServerError emptyResponse
            Just (Agenda.ServerCalendarItem {Agenda.content, Agenda.itemId}) -> do
              validation <- liftIO $ validateTripWrite principalUserId (Just itemId) content
              case validation of
                TripWriteValid -> do
                  result <- liftIO $ updateCalendarItem defaultCalendarStorageConfig principalUserId itemId content
                  case result of
                    Left CalendarItemNotFound -> notFound emptyResponse
                    Left _ -> internalServerError emptyResponse
                    Right updated -> ok (jsonResponse updated)
                TripWriteBadRequest message -> badRequest message
                TripWriteNotFound -> notFound emptyResponse
                TripWriteTechnicalFailure -> internalServerError emptyResponse
            Nothing ->
              case decode' (unBody rqBody) :: Maybe Agenda.ValidateRequest of
                Nothing -> badRequest "Unable to decode the body as a CalendarItem or ValidateRequest"
                Just (Agenda.ValidateRequest itemId minutes) -> do
                  result <- liftIO $ updateCalendarItemDuration defaultCalendarStorageConfig principalUserId itemId minutes
                  case result of
                    Left CalendarItemNotFound -> notFound emptyResponse
                    Left _ -> internalServerError emptyResponse
                    Right _ -> ok emptyResponse

    agendaDelete = do
      method DELETE
      path $ \itemId -> do
        nullDir
        result <- liftIO $ deleteCalendarItem defaultCalendarStorageConfig principalUserId itemId
        case result of
          Left CalendarItemNotFound -> notFound emptyResponse
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
