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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (Int64)
import Control.Monad (msum, mzero, join, foldM, when, mplus)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Either (either)
import Data.ByteString.Char8 (unpack)
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Happstack.Server (FilterMonad, Response, ServerPartT, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse, method, ok, internalServerError, notFound, dir, Method(GET, POST, DELETE, PUT), Conf(..), addCookie, mkCookie, CookieLife(Session, Expired), getHeaderM, unauthorized, requestEntityTooLarge, look)
import qualified Happstack.Server as HServer
import Happstack.Server.Internal.Cookie (Cookie(..), SameSite(..))
import Happstack.Server.Internal.MessageWrap (bodyInput, BodyPolicy)
import Model (NoteContent, ChecklistContent, Content, Identifiable(..))
import qualified AgendaModel as Agenda
import AgendaStorage (createCalendarItem, defaultCalendarStorageConfig, getCalendarItems, updateCalendarItemDuration, CalendarStorageError(..))
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
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 (writeFile)
import Filesystem.Path.CurrentOS    (commonPrefix, encodeString, decodeString, collapse, append)
import Auth (AuthRequest(..), AuthRequestError(..), AuthError(..), createUser, signinUser)
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

newtype AppConfigFile = AppConfigFile
  { appSession :: SessionConfigFile
  } deriving (Generic)

instance FromJSON AppConfigFile where
  parseJSON = withObject "AppConfigFile" $ \v -> AppConfigFile
    <$> v .: "session"

newtype AppContext = AppContext
  { sessionPrincipal :: SessionPrincipal
  }

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
  toServerResponse (TechnicalError _) = internalServerError $ jsonMessage "Unable to process authentication"

loadSessionConfigFromFile :: IO (Either String SessionConfig)
loadSessionConfigFromFile = do
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
            Right fileConfig -> pure $ Right (toSessionConfig secret fileConfig (parseBool =<< mCookieSecureRaw))


toSessionConfig :: String -> AppConfigFile -> Maybe Bool -> SessionConfig
toSessionConfig secret AppConfigFile {appSession = SessionConfigFile {sessionCookieNameFile, sessionAbsoluteTtlSecondsFile, sessionIdleTtlSecondsFile}} mCookieSecure =
  defaultSessionConfig
    { sessionSecret = secret
    , sessionCookieName = fromMaybe (sessionCookieName defaultSessionConfig) sessionCookieNameFile
    , sessionAbsoluteTtlSeconds = fromIntegral (fromMaybe (round (sessionAbsoluteTtlSeconds defaultSessionConfig)) sessionAbsoluteTtlSecondsFile)
    , sessionIdleTtlSeconds = fromIntegral (fromMaybe (round (sessionIdleTtlSeconds defaultSessionConfig)) sessionIdleTtlSecondsFile)
    , sessionCookieSecure = fromMaybe (sessionCookieSecure defaultSessionConfig) mCookieSecure
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
    sessionConfigResult <- loadSessionConfigFromFile
    case sessionConfigResult of
      Left err -> do
        putStrLn $ "[startup-error] " ++ err
        exitFailure
      Right sessionConfig -> do
        signupRateLimitState <- newMVar []
        tmpDir <- getTemporaryDirectory
        cd <- getCurrentDirectory
        sessionStore <- mkFileSessionStore (cd </> "data" </> "sessions") sessionConfig
        simpleHTTP nullConf { port = 8081 } $ do
            log "Incoming request" >> log "=========================END REQUEST====================\n"
            msum [ homePage
                 , apiController signupRateLimitState tmpDir sessionConfig sessionStore
                 , serveStaticResource
                 , mzero
                 ]

apiController :: MVar [UTCTime] -> FilePath -> SessionConfig -> SessionStore -> ServerPartT IO Response
apiController signupRateLimitState tmpDir sessionConfig sessionStore = dir "api" $ msum [ signupController signupRateLimitState tmpDir
                                                                                          , signinController sessionConfig sessionStore
                                                                                          , signoutController sessionConfig sessionStore
                                                                                          , requireAuth sessionConfig sessionStore noteController
                                                                                          , requireAuth sessionConfig sessionStore checklistController
                                                                                          , requireAuth sessionConfig sessionStore agendaController
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

signupController :: MVar [UTCTime] -> FilePath -> ServerPartT IO Response
signupController signupRateLimitState tmpDir = dir "signup" $ do
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
            res <- liftIO $ runExceptT $ createUser signupRequest
            either toServerResponse
                   (const $ ok emptyResponse)
                   res

signinController :: SessionConfig -> SessionStore -> ServerPartT IO Response
signinController sessionConfig sessionStore = dir "signin" $ do
  nullDir
  method POST
  withBusinessHandlingAndInput signinUser $ \authReq -> do
    sid <- liftIO $ createSessionForUser sessionStore (username authReq)
    let cookieValue = signSessionId (sessionSecret sessionConfig) sid
    addCookie Session (buildSessionCookie sessionConfig cookieValue)
    ok emptyResponse

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


withBusinessHandlingAndInput :: (FromJSON a, ToServerResponse e) => (a -> ExceptT e IO r) -> (a -> ServerPartT IO Response) -> ServerPartT IO Response
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
               (const $ onSuccess input)
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
       (x:_) -> Just $ drop (length targetPrefix) x

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

agendaController :: AppContext -> ServerPartT IO Response
agendaController _ = dir "v1" $ dir "calendar-items" $ msum [ agendaList
                                                            , agendaCreate
                                                            , agendaValidate
                                                            ]
  where
    agendaList = do
      nullDir
      method GET
      items <- liftIO $ getCalendarItems defaultCalendarStorageConfig
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
            Nothing -> badRequest "Unable to decode the body as a CalendarItem"
            Just (Agenda.NewCalendarItem {Agenda.content}) -> do
              created <- liftIO $ createCalendarItem defaultCalendarStorageConfig content
              ok (jsonResponse created)
            Just (Agenda.ServerCalendarItem {Agenda.content}) -> do
              created <- liftIO $ createCalendarItem defaultCalendarStorageConfig content
              ok (jsonResponse created)

    agendaValidate = path $ \itemId -> dir "validate" $ do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody =
          case decode' (unBody rqBody) :: Maybe Agenda.ValidateRequest of
            Nothing -> badRequest "Unable to decode the body as a ValidateRequest"
            Just (Agenda.ValidateRequest minutes) -> do
              result <- liftIO $ updateCalendarItemDuration defaultCalendarStorageConfig itemId minutes
              case result of
                Left CalendarItemNotFound -> notFound emptyResponse
                Left _ -> internalServerError emptyResponse
                Right _ -> ok emptyResponse
      maybe (badRequest "Empty body")
            handleBody
            body

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
