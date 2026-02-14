{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( runApp
    ) where

import Prelude hiding (log, writeFile)
import Data.Aeson (ToJSON(toJSON), FromJSON, decode, encode, decode')
import Data.Function ((&))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Control.Monad (msum, mzero, join, foldM, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Either (either)
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import qualified Data.ByteString.Lazy as BSL (length)
import Happstack.Server (FilterMonad, Response, ServerPartT, RqBody, takeRequestBody, unBody, rqBody, decodeBody, askRq, defaultBodyPolicy, nullDir, path, serveFileFrom, guessContentTypeM, mimeTypes, uriRest, nullConf, simpleHTTP, toResponse, method, ok, internalServerError, notFound, dir, Method(GET, POST, DELETE, PUT), Conf(..))
import qualified Happstack.Server as HServer
import Model (NoteContent, ChecklistContent, Content, Identifiable(..))
import qualified CrudStorage
import Crud
import NoteCrud (NoteServiceConfig(..), defaultNoteServiceConfig)
import ChecklistCrud (ChecklistServiceConfig(..), defaultChecklistServiceConfig)
import System.Directory (doesFileExist, getCurrentDirectory, canonicalizePath)
import System.FilePath ((</>), pathSeparator)
import System.IO (hFlush, stdout)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 (writeFile)
import Filesystem.Path.CurrentOS    (commonPrefix, encodeString, decodeString, collapse, append)
import qualified Auth (SignupRequest (SignupRequest), SignupRequestError(..), SignupError(..), createUser)

type AppM a = ExceptT String (ServerPartT IO) a

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

instance ToServerResponse Auth.SignupError where
  toServerResponse (Auth.BadRequest br) = badRequest errorStr
    where errorStr = case br of
                     Auth.EmptyUsername -> "Username cannot be empty"
                     Auth.UsernameDoesNotRespectPattern -> "Username has forbidden characters"
                     Auth.EmptyPassword -> "Password cannot be empty"
                     Auth.PasswordTooShort -> "Password is too short"
                     Auth.UsernameTooShort -> "Username is too short"
                     Auth.UsernameTooLong -> "Username is too long"
  toServerResponse Auth.UserAlreadyExists = badRequest "Unable to create user"
  toServerResponse (Auth.TechnicalError _) = internalServerError $ toResponse ("Unable to create user" :: String)

runApp :: IO ()
runApp = do
    putStrLn "running server"
    signupRateLimitState <- newMVar []
    simpleHTTP nullConf { port = 8081 } $ do
        log "Incoming request" >> log "=========================END REQUEST====================\n"
        msum [ homePage
             , apiController signupRateLimitState
             , serveStaticResource
             , mzero
             ]

apiController :: MVar [UTCTime] -> ServerPartT IO Response
apiController signupRateLimitState = dir "api" $ msum [ noteController
                                                       , checklistController
                                                       , signupController signupRateLimitState
                                                       ]

homePage :: ServerPartT IO Response
homePage = do
    nullDir
    cd <- liftIO getCurrentDirectory
    serveFileFrom (cd </> "static/") (guessContentTypeM mimeTypes) "index.html"

maxSignupBodyBytes :: Int
maxSignupBodyBytes = 4096

isSignupBodyTooLarge :: RqBody -> Bool
isSignupBodyTooLarge body = fromIntegral (BSL.length $ unBody body) > maxSignupBodyBytes

signupController :: MVar [UTCTime] -> ServerPartT IO Response
signupController signupRateLimitState = dir "signup" $ do
    nullDir
    method POST
    allowed <- liftIO $ allowSignupRequest signupRateLimitState
    if not allowed
      then tooManyRequests "Too many signup attempts. Please retry later."
      else do
        liftIO $ putStrLn "Reading signup body"
        body <- askRq >>= takeRequestBody
        maybe (badRequest "Empty body")
              handleBody
              body
    where handleBody :: RqBody -> ServerPartT IO Response --AppM Response
          handleBody body =
            if isSignupBodyTooLarge body
              then badRequest "Body too large"
              else maybe (badRequest "Unable to decode the body as a SignupData")
                         doCreateUser
                         (decode $ unBody body)

          doCreateUser :: Auth.SignupRequest -> ServerPartT IO Response --AppM Response
          doCreateUser signupRequest = do
            res <- liftIO $ runExceptT $ Auth.createUser signupRequest
            either toServerResponse
                   (ok . toResponse)
                   res



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


noteController :: ServerPartT IO Response
noteController = dir "note" noteHandlers
    where
        noteHandlers = msum $ defaultNoteServiceConfig <%> [ crudGet
                                                           , crudPost
                                                           , crudDelete
                                                           , crudPut
                                                           ]
checklistController :: ServerPartT IO Response
checklistController = dir "checklist" $
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
