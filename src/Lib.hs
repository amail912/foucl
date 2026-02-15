{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( runApp
    ) where

import Prelude hiding (log, writeFile)
import Data.Aeson (ToJSON(toJSON), FromJSON, decode, encode, decode', eitherDecodeFileStrict', (.:), (.:?), withObject)
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
import qualified Data.ByteString.Char8 as BS8
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Happstack.Server (FilterMonad, Response, ServerPartT, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse, method, ok, internalServerError, notFound, dir, Method(GET, POST, DELETE, PUT), Conf(..), addCookie, mkCookie, CookieLife(Session, Expired), getHeaderM)
import qualified Happstack.Server as HServer
import qualified Happstack.Server.Internal.Cookie as HCookie
import Happstack.Server.Internal.MessageWrap (bodyInput, BodyPolicy)
import Model (NoteContent, ChecklistContent, Content, Identifiable(..))
import qualified CrudStorage
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
import qualified Auth (AuthRequest(..), AuthRequestError(..), AuthError(..), createUser, signinUser)
import qualified Session

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

data AppConfigFile = AppConfigFile
  { appSession :: !SessionConfigFile
  } deriving (Generic)

instance FromJSON AppConfigFile where
  parseJSON = withObject "AppConfigFile" $ \v -> AppConfigFile
    <$> v .: "session"

data AppContext = AppContext
  { sessionPrincipal :: !Session.SessionPrincipal
  }

badRequest :: FilterMonad Response m => String -> m Response
badRequest =  HServer.badRequest . toResponse

jsonResponse :: ToJSON a => a -> Response
jsonResponse = toResponse . encode

stringResponse :: String -> Response
stringResponse = toResponse

emptyResponse :: Response
emptyResponse = toResponse ()

class ToServerResponse e where
  toServerResponse :: Monad m => e -> ServerPartT m Response

instance ToServerResponse Auth.AuthError where
  toServerResponse (Auth.BadRequest br) = badRequest errorStr
    where errorStr = case br of
                     Auth.EmptyUsername -> "Username cannot be empty"
                     Auth.UsernameDoesNotRespectPattern -> "Username has forbidden characters"
                     Auth.EmptyPassword -> "Password cannot be empty"
                     Auth.PasswordTooShort -> "Password is too short"
                     Auth.UsernameTooShort -> "Username is too short"
                     Auth.UsernameTooLong -> "Username is too long"
  toServerResponse Auth.UserAlreadyExists = badRequest "Unable to create user"
  toServerResponse Auth.InvalidCredentials = HServer.unauthorized $ toResponse ("Invalid credentials" :: String)
  toServerResponse (Auth.TechnicalError _) = internalServerError $ toResponse ("Unable to process authentication" :: String)

loadSessionConfigFromFile :: IO (Either String Session.SessionConfig)
loadSessionConfigFromFile = do
  mSecret <- lookupEnv "FOUCL_SESSION_SECRET"
  mConfigPath <- lookupEnv "FOUCL_CONFIG_FILE"
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
            Right fileConfig -> pure $ Right (toSessionConfig secret fileConfig)


toSessionConfig :: String -> AppConfigFile -> Session.SessionConfig
toSessionConfig secret AppConfigFile {appSession = SessionConfigFile {sessionCookieNameFile, sessionAbsoluteTtlSecondsFile, sessionIdleTtlSecondsFile}} =
  Session.defaultSessionConfig
    { Session.sessionSecret = secret
    , Session.sessionCookieName = fromMaybe (Session.sessionCookieName Session.defaultSessionConfig) sessionCookieNameFile
    , Session.sessionAbsoluteTtlSeconds = fromIntegral (fromMaybe (round (Session.sessionAbsoluteTtlSeconds Session.defaultSessionConfig)) sessionAbsoluteTtlSecondsFile)
    , Session.sessionIdleTtlSeconds = fromIntegral (fromMaybe (round (Session.sessionIdleTtlSeconds Session.defaultSessionConfig)) sessionIdleTtlSecondsFile)
    }

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
        sessionStore <- Session.mkFileSessionStore (cd </> "data" </> "sessions") sessionConfig
        simpleHTTP nullConf { port = 8081 } $ do
            log "Incoming request" >> log "=========================END REQUEST====================\n"
            msum [ homePage
                 , apiController signupRateLimitState tmpDir sessionConfig sessionStore
                 , serveStaticResource
                 , mzero
                 ]

apiController :: MVar [UTCTime] -> FilePath -> Session.SessionConfig -> Session.SessionStore -> ServerPartT IO Response
apiController signupRateLimitState tmpDir sessionConfig sessionStore = dir "api" $ msum [ requireAuth sessionConfig sessionStore noteController
                                                                                          , requireAuth sessionConfig sessionStore checklistController
                                                                                          , signupController signupRateLimitState tmpDir
                                                                                          , signinController sessionConfig sessionStore
                                                                                          , signoutController sessionConfig sessionStore
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
    allowed <- liftIO $ allowSignupRequest signupRateLimitState
    if not allowed
      then tooManyRequests "Too many signup attempts. Please retry later."
      else do
        rq <- askRq
        (_, mBodyErr) <- liftIO $ bodyInput (signupBodyPolicy tmpDir) rq
        case mBodyErr of
          Just bodyErr | isTooLargeBodyError bodyErr -> HServer.requestEntityTooLarge $ toResponse ("Body too large" :: String)
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

          doCreateUser :: Auth.AuthRequest -> ServerPartT IO Response --AppM Response
          doCreateUser signupRequest = do
            res <- liftIO $ runExceptT $ Auth.createUser signupRequest
            either toServerResponse
                   (ok . toResponse)
                   res

signinController :: Session.SessionConfig -> Session.SessionStore -> ServerPartT IO Response
signinController sessionConfig sessionStore = dir "signin" $ do
  nullDir
  method POST
  withBusinessHandlingAndInput Auth.signinUser $ \authReq -> do
    sid <- liftIO $ Session.createSessionForUser sessionStore (Auth.username authReq)
    let cookieValue = Session.signSessionId (Session.sessionSecret sessionConfig) sid
    addCookie Session (buildSessionCookie sessionConfig cookieValue)
    ok $ toResponse ()

signoutController :: Session.SessionConfig -> Session.SessionStore -> ServerPartT IO Response
signoutController sessionConfig sessionStore = dir "signout" $ do
  nullDir
  method POST
  revokeAll <- isSignoutAllRequested
  mToken <- getSessionCookieValue sessionConfig
  case mToken >>= Session.verifyAndExtractSessionId (Session.sessionSecret sessionConfig) of
    Nothing -> HServer.unauthorized $ toResponse ("Not authenticated" :: String)
    Just sid -> do
      _ <- liftIO $ if revokeAll then Session.revokeAllForSession sessionStore sid else Session.revokeSession sessionStore sid
      addCookie Expired (buildSessionCookie sessionConfig "")
      ok $ toResponse ()



isSignoutAllRequested :: ServerPartT IO Bool
isSignoutAllRequested = do
  mRaw <- (Just <$> HServer.look "all") `mplus` pure Nothing
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
                   (const $ ok $ toResponse ())
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

buildSessionCookie :: Session.SessionConfig -> String -> HCookie.Cookie
buildSessionCookie sessionConfig cookieValue =
  (mkCookie (Session.sessionCookieName sessionConfig) cookieValue)
    { HCookie.secure = True
    , HCookie.httpOnly = True
    , HCookie.sameSite = HCookie.SameSiteLax
    }

getSessionCookieValue :: Session.SessionConfig -> ServerPartT IO (Maybe String)
getSessionCookieValue sessionConfig = do
  mCookieHeader <- getHeaderM "cookie"
  pure $ mCookieHeader >>= extractCookie (Session.sessionCookieName sessionConfig) . BS8.unpack

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

requireAuth :: Session.SessionConfig -> Session.SessionStore -> (AppContext -> ServerPartT IO Response) -> ServerPartT IO Response
requireAuth sessionConfig sessionStore handler = do
  mToken <- getSessionCookieValue sessionConfig
  case mToken >>= Session.verifyAndExtractSessionId (Session.sessionSecret sessionConfig) of
    Nothing -> HServer.unauthorized $ toResponse ("Not authenticated" :: String)
    Just sid -> do
      mPrincipal <- liftIO $ Session.resolveSession sessionStore sid
      case mPrincipal of
        Nothing -> HServer.unauthorized $ toResponse ("Not authenticated" :: String)
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
    fmap handleBody body `orElse` ok (stringResponse "NoBody")

createNoteContent :: CRUDEngine crudType a => crudType -> a -> ServerPartT IO Response
createNoteContent crudConfig noteContent = do
    recover (logThenGenericInternalError crudConfig) (ok . jsonResponse) $ CrudStorage.createItem crudConfig noteContent

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
        recover (handleDeletionError pathId) (\() -> ok emptyResponse) $ CrudStorage.deleteItem crudConfig pathId)

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
    fmap handleBody body `orElse` ok (stringResponse "NoBody") -- do not send back ok when there is no body

handleUpdate :: CRUDEngine crudType a => crudType -> Identifiable a -> ServerPartT IO Response
handleUpdate crudConfig update =
    recoverWith (const.notFound.stringResponse $ "Unable to find storage dir")
                (ok.jsonResponse <$> CrudStorage.modifyItem crudConfig update)

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
