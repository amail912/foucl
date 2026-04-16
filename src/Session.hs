{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Session
  ( SessionConfig(..)
  , defaultSessionConfig
  , SessionPrincipal(..)
  , SessionState(..)
  , SessionHandle(..)
  , UserStateBinding(..)
  , SessionRepository(..)
  , SessionStore(..)
  , mkSessionStore
  , mkFileSessionStore
  , signSessionId
  , verifyAndExtractSessionId
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), encode, decode, object, withObject)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import qualified Data.ByteString.Lazy as BL
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Char (isHexDigit, toLower)
import Data.Maybe (isJust, isNothing)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Repository (RepositoryError(..))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath ((</>), takeDirectory)
import System.IO (openTempFile, hClose)
import System.IO.Error (isDoesNotExistError)
import qualified Control.Exception as Ex


data SessionConfig = SessionConfig
  { sessionCookieName :: !String
  , sessionSecret :: !String
  , sessionAbsoluteTtlSeconds :: !NominalDiffTime
  , sessionIdleTtlSeconds :: !NominalDiffTime
  , sessionCookieSecure :: !Bool
  }

defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
  { sessionCookieName = "foucl_session"
  , sessionSecret = "change-me-in-prod"
  , sessionAbsoluteTtlSeconds = 7 * 24 * 60 * 60
  , sessionIdleTtlSeconds = 24 * 60 * 60
  , sessionCookieSecure = True
  }


data SessionPrincipal = SessionPrincipal
  { principalUserId :: !String
  , principalSessionId :: !String
  , principalSessionStateId :: !String
  }


data SessionState = SessionState
  { stateId :: !String
  , stateUserId :: !String
  , stateCreatedAt :: !UTCTime
  , stateExpiresAt :: !UTCTime
  , stateIdleExpiresAt :: !UTCTime
  , stateRevokedAt :: !(Maybe UTCTime)
  }

instance ToJSON SessionState where
  toJSON SessionState {stateId, stateUserId, stateCreatedAt, stateExpiresAt, stateIdleExpiresAt, stateRevokedAt} =
    object [ "stateId" .= stateId
           , "stateUserId" .= stateUserId
           , "stateCreatedAt" .= stateCreatedAt
           , "stateExpiresAt" .= stateExpiresAt
           , "stateIdleExpiresAt" .= stateIdleExpiresAt
           , "stateRevokedAt" .= stateRevokedAt
           ]

instance FromJSON SessionState where
  parseJSON = withObject "SessionState" $ \v -> SessionState
    <$> v .: "stateId"
    <*> v .: "stateUserId"
    <*> v .: "stateCreatedAt"
    <*> v .: "stateExpiresAt"
    <*> v .: "stateIdleExpiresAt"
    <*> v .: "stateRevokedAt"


data SessionHandle = SessionHandle
  { handleSessionId :: !String
  , handleStateId :: !String
  , handleIssuedAt :: !UTCTime
  , handleRevokedAt :: !(Maybe UTCTime)
  }

instance ToJSON SessionHandle where
  toJSON SessionHandle {handleSessionId, handleStateId, handleIssuedAt, handleRevokedAt} =
    object [ "handleSessionId" .= handleSessionId
           , "handleStateId" .= handleStateId
           , "handleIssuedAt" .= handleIssuedAt
           , "handleRevokedAt" .= handleRevokedAt
           ]

instance FromJSON SessionHandle where
  parseJSON = withObject "SessionHandle" $ \v -> SessionHandle
    <$> v .: "handleSessionId"
    <*> v .: "handleStateId"
    <*> v .: "handleIssuedAt"
    <*> v .: "handleRevokedAt"


newtype UserStateBinding = UserStateBinding { boundStateId :: String }

instance ToJSON UserStateBinding where
  toJSON UserStateBinding {boundStateId} = object ["boundStateId" .= boundStateId]

instance FromJSON UserStateBinding where
  parseJSON = withObject "UserStateBinding" $ \v -> UserStateBinding <$> v .: "boundStateId"


data SessionRepository = SessionRepository
  { repoCreateSessionHandle :: SessionHandle -> ExceptT RepositoryError IO ()
  , repoLoadSessionHandleBySessionId :: String -> ExceptT RepositoryError IO SessionHandle
  , repoUpdateSessionHandle :: SessionHandle -> ExceptT RepositoryError IO ()
  , repoDeleteSessionHandleBySessionId :: String -> ExceptT RepositoryError IO ()
  , repoCreateSessionState :: SessionState -> ExceptT RepositoryError IO ()
  , repoLoadSessionStateByStateId :: String -> ExceptT RepositoryError IO SessionState
  , repoUpdateSessionState :: SessionState -> ExceptT RepositoryError IO ()
  , repoDeleteSessionStateByStateId :: String -> ExceptT RepositoryError IO ()
  , repoCreateUserStateBinding :: String -> UserStateBinding -> ExceptT RepositoryError IO ()
  , repoLoadUserStateBindingByUserId :: String -> ExceptT RepositoryError IO UserStateBinding
  , repoDeleteUserStateBindingByUserId :: String -> ExceptT RepositoryError IO ()
  , repoDeleteAllUserStateBindingsForUser :: String -> ExceptT RepositoryError IO ()
  }


data SessionStore = SessionStore
  { createSessionForUser :: String -> IO String
  , resolveSession :: String -> IO (Maybe SessionPrincipal)
  , revokeSession :: String -> IO Bool
  , revokeAllForSession :: String -> IO Bool
  }

mkSessionStore :: SessionRepository -> SessionConfig -> SessionStore
mkSessionStore repo config =
  SessionStore
    { createSessionForUser = createSessionForUserImpl repo config
    , resolveSession = resolveSessionImpl repo config
    , revokeSession = revokeSessionImpl repo
    , revokeAllForSession = revokeAllForSessionImpl repo
    }

mkFileSessionStore :: FilePath -> SessionConfig -> IO SessionStore
mkFileSessionStore baseDir config = do
  createDirectoryIfMissing True (baseDir </> "handles")
  createDirectoryIfMissing True (baseDir </> "states")
  createDirectoryIfMissing True (baseDir </> "users")
  pure (mkSessionStore (mkFilesystemSessionRepository baseDir) config)

mkFilesystemSessionRepository :: FilePath -> SessionRepository
mkFilesystemSessionRepository baseDir =
  SessionRepository
    { repoCreateSessionHandle = fsCreateSessionHandle baseDir
    , repoLoadSessionHandleBySessionId = fsLoadSessionHandleBySessionId baseDir
    , repoUpdateSessionHandle = fsUpdateSessionHandle baseDir
    , repoDeleteSessionHandleBySessionId = fsDeleteSessionHandleBySessionId baseDir
    , repoCreateSessionState = fsCreateSessionState baseDir
    , repoLoadSessionStateByStateId = fsLoadSessionStateByStateId baseDir
    , repoUpdateSessionState = fsUpdateSessionState baseDir
    , repoDeleteSessionStateByStateId = fsDeleteSessionStateByStateId baseDir
    , repoCreateUserStateBinding = fsCreateUserStateBinding baseDir
    , repoLoadUserStateBindingByUserId = fsLoadUserStateBindingByUserId baseDir
    , repoDeleteUserStateBindingByUserId = fsDeleteUserStateBindingByUserId baseDir
    , repoDeleteAllUserStateBindingsForUser = fsDeleteAllUserStateBindingsForUser baseDir
    }

createSessionForUserImpl :: SessionRepository -> SessionConfig -> String -> IO String
createSessionForUserImpl repo config userId = do
  now <- getCurrentTime
  stateResult <- runExceptT $ getOrCreateState repo config userId now
  case stateResult of
    Left _ -> fail "Unable to create session state"
    Right state -> do
      sid <- toString <$> nextRandom
      let handle = SessionHandle
            { handleSessionId = sid
            , handleStateId = stateId state
            , handleIssuedAt = now
            , handleRevokedAt = Nothing
            }
      handleResult <- runExceptT $ repoCreateSessionHandle repo handle
      case handleResult of
        Left _ -> fail "Unable to create session handle"
        Right () -> pure sid

resolveSessionImpl :: SessionRepository -> SessionConfig -> String -> IO (Maybe SessionPrincipal)
resolveSessionImpl repo config sid = do
  mHandle <- loadHandleMaybe repo sid
  case mHandle of
    Nothing -> pure Nothing
    Just h | isJust (handleRevokedAt h) -> pure Nothing
    Just h -> do
      mState <- loadStateMaybe repo (handleStateId h)
      case mState of
        Nothing -> pure Nothing
        Just st -> do
          now <- getCurrentTime
          if isStateValid now st
            then do
              stateTouched <- touchState repo config now st
              if stateTouched
                then pure $ Just SessionPrincipal
                  { principalUserId = stateUserId st
                  , principalSessionId = sid
                  , principalSessionStateId = stateId st
                  }
                else pure Nothing
            else pure Nothing

revokeSessionImpl :: SessionRepository -> String -> IO Bool
revokeSessionImpl repo sid = do
  handleResult <- runExceptT $ repoLoadSessionHandleBySessionId repo sid
  case handleResult of
    Left NotFound -> pure False
    Left _ -> pure False
    Right h | isJust (handleRevokedAt h) -> pure True
    Right h -> do
      now <- getCurrentTime
      updateResult <- runExceptT $ repoUpdateSessionHandle repo h { handleRevokedAt = Just now }
      pure (isSuccess updateResult)

revokeAllForSessionImpl :: SessionRepository -> String -> IO Bool
revokeAllForSessionImpl repo sid = do
  handleResult <- runExceptT $ repoLoadSessionHandleBySessionId repo sid
  case handleResult of
    Left NotFound -> pure False
    Left _ -> pure False
    Right h -> do
      stateResult <- runExceptT $ repoLoadSessionStateByStateId repo (handleStateId h)
      case stateResult of
        Left NotFound -> pure False
        Left _ -> pure False
        Right st -> do
          now <- getCurrentTime
          revokeStateResult <- runExceptT $ repoUpdateSessionState repo st { stateRevokedAt = Just now }
          if not (isSuccess revokeStateResult)
            then pure False
            else do
              deleteResult <- runExceptT $ repoDeleteAllUserStateBindingsForUser repo (stateUserId st)
              case deleteResult of
                Left NotFound -> pure True
                Left _ -> pure False
                Right () -> pure True

loadHandleMaybe :: SessionRepository -> String -> IO (Maybe SessionHandle)
loadHandleMaybe repo sid = do
  result <- runExceptT $ repoLoadSessionHandleBySessionId repo sid
  pure $ either (const Nothing) Just result

loadStateMaybe :: SessionRepository -> String -> IO (Maybe SessionState)
loadStateMaybe repo stId = do
  result <- runExceptT $ repoLoadSessionStateByStateId repo stId
  pure $ either (const Nothing) Just result

isSuccess :: Either a b -> Bool
isSuccess (Right _) = True
isSuccess _ = False

isStateValid :: UTCTime -> SessionState -> Bool
isStateValid now st =
  stateExpiresAt st > now && stateIdleExpiresAt st > now && isNothing (stateRevokedAt st)

touchState :: SessionRepository -> SessionConfig -> UTCTime -> SessionState -> IO Bool
touchState repo SessionConfig {sessionIdleTtlSeconds} now st = do
  let newIdle = min (stateExpiresAt st) (addUTCTime sessionIdleTtlSeconds now)
  updateResult <- runExceptT $ repoUpdateSessionState repo st { stateIdleExpiresAt = newIdle }
  pure (isSuccess updateResult)

getOrCreateState :: SessionRepository -> SessionConfig -> String -> UTCTime -> ExceptT RepositoryError IO SessionState
getOrCreateState repo config@SessionConfig {sessionAbsoluteTtlSeconds, sessionIdleTtlSeconds} userId now = do
  mBinding <- loadBindingMaybe repo userId
  case mBinding of
    Just UserStateBinding {boundStateId} -> do
      mState <- loadStateByIdMaybe repo boundStateId
      case mState of
        Just st | isStateValid now st -> pure st
        _ -> createNewState
    Nothing -> createNewState
  where
    createNewState = do
      stId <- liftIO (toString <$> nextRandom)
      let st = SessionState
            { stateId = stId
            , stateUserId = userId
            , stateCreatedAt = now
            , stateExpiresAt = addUTCTime sessionAbsoluteTtlSeconds now
            , stateIdleExpiresAt = addUTCTime sessionIdleTtlSeconds now
            , stateRevokedAt = Nothing
            }
      repoCreateSessionState repo st
      createBindingResult <- liftIO $ runExceptT $ repoCreateUserStateBinding repo userId UserStateBinding {boundStateId = stId}
      case createBindingResult of
        Right () -> pure st
        Left AlreadyExists -> do
          _ <- liftIO $ runExceptT $ repoDeleteUserStateBindingByUserId repo userId
          repoCreateUserStateBinding repo userId UserStateBinding {boundStateId = stId}
          pure st
        Left err -> throwError err

loadBindingMaybe :: SessionRepository -> String -> ExceptT RepositoryError IO (Maybe UserStateBinding)
loadBindingMaybe repo userId = do
  result <- liftIO $ runExceptT $ repoLoadUserStateBindingByUserId repo userId
  case result of
    Left NotFound -> pure Nothing
    Left err -> throwError err
    Right binding -> pure (Just binding)

loadStateByIdMaybe :: SessionRepository -> String -> ExceptT RepositoryError IO (Maybe SessionState)
loadStateByIdMaybe repo stId = do
  result <- liftIO $ runExceptT $ repoLoadSessionStateByStateId repo stId
  case result of
    Left NotFound -> pure Nothing
    Left err -> throwError err
    Right st -> pure (Just st)

fsCreateSessionHandle :: FilePath -> SessionHandle -> ExceptT RepositoryError IO ()
fsCreateSessionHandle baseDir handle = do
  let path = handlePath baseDir (handleSessionId handle)
  exists <- ioOr ReadFailure (doesFileExist path)
  if exists
    then throwError AlreadyExists
    else ioOr WriteFailure (writeJsonAtomic path handle)

fsLoadSessionHandleBySessionId :: FilePath -> String -> ExceptT RepositoryError IO SessionHandle
fsLoadSessionHandleBySessionId baseDir sid =
  readJsonFileOrNotFound (handlePath baseDir sid)

fsUpdateSessionHandle :: FilePath -> SessionHandle -> ExceptT RepositoryError IO ()
fsUpdateSessionHandle baseDir handle = do
  let path = handlePath baseDir (handleSessionId handle)
  exists <- ioOr ReadFailure (doesFileExist path)
  if not exists
    then throwError NotFound
    else ioOr WriteFailure (writeJsonAtomic path handle)

fsDeleteSessionHandleBySessionId :: FilePath -> String -> ExceptT RepositoryError IO ()
fsDeleteSessionHandleBySessionId baseDir sid =
  deleteFileOrNotFound (handlePath baseDir sid)

fsCreateSessionState :: FilePath -> SessionState -> ExceptT RepositoryError IO ()
fsCreateSessionState baseDir st = do
  let path = statePath baseDir (stateId st)
  exists <- ioOr ReadFailure (doesFileExist path)
  if exists
    then throwError AlreadyExists
    else ioOr WriteFailure (writeJsonAtomic path st)

fsLoadSessionStateByStateId :: FilePath -> String -> ExceptT RepositoryError IO SessionState
fsLoadSessionStateByStateId baseDir stId =
  readJsonFileOrNotFound (statePath baseDir stId)

fsUpdateSessionState :: FilePath -> SessionState -> ExceptT RepositoryError IO ()
fsUpdateSessionState baseDir st = do
  let path = statePath baseDir (stateId st)
  exists <- ioOr ReadFailure (doesFileExist path)
  if not exists
    then throwError NotFound
    else ioOr WriteFailure (writeJsonAtomic path st)

fsDeleteSessionStateByStateId :: FilePath -> String -> ExceptT RepositoryError IO ()
fsDeleteSessionStateByStateId baseDir stId =
  deleteFileOrNotFound (statePath baseDir stId)

fsCreateUserStateBinding :: FilePath -> String -> UserStateBinding -> ExceptT RepositoryError IO ()
fsCreateUserStateBinding baseDir userId binding = do
  let path = userBindingPath baseDir userId
  exists <- ioOr ReadFailure (doesFileExist path)
  if exists
    then throwError AlreadyExists
    else ioOr WriteFailure (writeJsonAtomic path binding)

fsLoadUserStateBindingByUserId :: FilePath -> String -> ExceptT RepositoryError IO UserStateBinding
fsLoadUserStateBindingByUserId baseDir userId =
  readJsonFileOrNotFound (userBindingPath baseDir userId)

fsDeleteUserStateBindingByUserId :: FilePath -> String -> ExceptT RepositoryError IO ()
fsDeleteUserStateBindingByUserId baseDir userId =
  deleteFileOrNotFound (userBindingPath baseDir userId)

fsDeleteAllUserStateBindingsForUser :: FilePath -> String -> ExceptT RepositoryError IO ()
fsDeleteAllUserStateBindingsForUser baseDir userId = do
  let path = userBindingPath baseDir userId
  exists <- ioOr ReadFailure (doesFileExist path)
  if exists
    then deleteFileOrNotFound path
    else pure ()

handlePath :: FilePath -> String -> FilePath
handlePath baseDir sid = baseDir </> "handles" </> sid ++ ".json"

statePath :: FilePath -> String -> FilePath
statePath baseDir stId = baseDir </> "states" </> stId ++ ".json"

userBindingPath :: FilePath -> String -> FilePath
userBindingPath baseDir userId = baseDir </> "users" </> userId ++ ".json"

readJsonFileOrNotFound :: FromJSON a => FilePath -> ExceptT RepositoryError IO a
readJsonFileOrNotFound fp = do
  exists <- ioOr ReadFailure (doesFileExist fp)
  if not exists
    then throwError NotFound
    else do
      content <- ioOr ReadFailure (BL.readFile fp)
      case decode content of
        Nothing -> throwError ReadFailure
        Just parsed -> pure parsed

deleteFileOrNotFound :: FilePath -> ExceptT RepositoryError IO ()
deleteFileOrNotFound fp = do
  deleteResult <- liftIO $ Ex.try (removeFile fp) :: ExceptT RepositoryError IO (Either Ex.IOException ())
  case deleteResult of
    Left err
      | isDoesNotExistError err -> throwError NotFound
      | otherwise -> throwError WriteFailure
    Right () -> pure ()

ioOr :: RepositoryError -> IO a -> ExceptT RepositoryError IO a
ioOr err action = do
  result <- liftIO (Ex.try action)
  case result of
    Left (_ :: Ex.IOException) -> throwError err
    Right value -> pure value

writeJsonAtomic :: ToJSON a => FilePath -> a -> IO ()
writeJsonAtomic path value = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  (tmpPath, h) <- openTempFile dir "tmp-session"
  BL.hPut h (encode value)
  hClose h
  renameFile tmpPath path

signSessionId :: String -> String -> String
signSessionId secret sid = sid ++ "." ++ signature
  where
    signature = digestFor secret sid

verifyAndExtractSessionId :: String -> String -> Maybe String
verifyAndExtractSessionId secret token =
  case break (== '.') token of
    (sid, '.':sig)
      | not (null sid)
      , isHex sig
      , constantTimeEq (map toLower sig) expected -> Just sid
      where expected = digestFor secret sid
    _ -> Nothing
  where
    isHex = all isHexDigit

constantTimeEq :: String -> String -> Bool
constantTimeEq a b = constEq (pack a) (pack b)

digestFor :: String -> String -> String
digestFor secret sid = unpack $ convertToBase Base16 mac
  where
    mac :: HMAC SHA256
    mac = hmac (pack secret) (pack sid)
