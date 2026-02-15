{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Session
  ( SessionConfig(..)
  , defaultSessionConfig
  , SessionPrincipal(..)
  , SessionStore(..)
  , mkFileSessionStore
  , signSessionId
  , verifyAndExtractSessionId
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), encode, decode, object, withObject)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import qualified Data.ByteString.Lazy as BL
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Char (isHexDigit, toLower)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile, listDirectory)
import System.FilePath ((</>), takeDirectory)
import System.IO (openTempFile, hClose)


data SessionConfig = SessionConfig
  { sessionCookieName :: !String
  , sessionSecret :: !String
  , sessionAbsoluteTtlSeconds :: !NominalDiffTime
  , sessionIdleTtlSeconds :: !NominalDiffTime
  }

defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
  { sessionCookieName = "foucl_session"
  , sessionSecret = "change-me-in-prod"
  , sessionAbsoluteTtlSeconds = 7 * 24 * 60 * 60
  , sessionIdleTtlSeconds = 24 * 60 * 60
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


data UserStateBinding = UserStateBinding { boundStateId :: !String }

instance ToJSON UserStateBinding where
  toJSON UserStateBinding {boundStateId} = object ["boundStateId" .= boundStateId]

instance FromJSON UserStateBinding where
  parseJSON = withObject "UserStateBinding" $ \v -> UserStateBinding <$> v .: "boundStateId"


data SessionStore = SessionStore
  { createSessionForUser :: String -> IO String
  , resolveSession :: String -> IO (Maybe SessionPrincipal)
  , revokeSession :: String -> IO Bool
  , revokeAllForSession :: String -> IO Bool
  }

mkFileSessionStore :: FilePath -> SessionConfig -> IO SessionStore
mkFileSessionStore baseDir config = do
  createDirectoryIfMissing True (baseDir </> "handles")
  createDirectoryIfMissing True (baseDir </> "states")
  createDirectoryIfMissing True (baseDir </> "users")
  pure SessionStore
    { createSessionForUser = createSessionForUserImpl baseDir config
    , resolveSession = resolveSessionImpl baseDir config
    , revokeSession = revokeSessionImpl baseDir
    , revokeAllForSession = revokeAllForSessionImpl baseDir
    }

createSessionForUserImpl :: FilePath -> SessionConfig -> String -> IO String
createSessionForUserImpl baseDir config userId = do
  now <- getCurrentTime
  state <- getOrCreateState baseDir config userId now
  sid <- toString <$> nextRandom
  let handle = SessionHandle { handleSessionId = sid, handleStateId = stateId state, handleIssuedAt = now, handleRevokedAt = Nothing }
  writeJsonAtomic (handlePath baseDir sid) handle
  pure sid

resolveSessionImpl :: FilePath -> SessionConfig -> String -> IO (Maybe SessionPrincipal)
resolveSessionImpl baseDir config sid = do
  mHandle <- readJsonFile (handlePath baseDir sid)
  case mHandle of
    Nothing -> pure Nothing
    Just h | isJust (handleRevokedAt h) -> pure Nothing
    Just h -> do
      mState <- readJsonFile (statePath baseDir $ handleStateId h)
      case mState of
        Nothing -> pure Nothing
        Just st -> do
          now <- getCurrentTime
          if isStateValid now st
            then do
              touchState baseDir config now st
              pure $ Just SessionPrincipal { principalUserId = stateUserId st, principalSessionId = sid, principalSessionStateId = stateId st }
            else pure Nothing

revokeSessionImpl :: FilePath -> String -> IO Bool
revokeSessionImpl baseDir sid = do
  mHandle <- readJsonFile (handlePath baseDir sid)
  case mHandle of
    Nothing -> pure False
    Just h -> do
      now <- getCurrentTime
      writeJsonAtomic (handlePath baseDir sid) h { handleRevokedAt = Just now }
      pure True

revokeAllForSessionImpl :: FilePath -> String -> IO Bool
revokeAllForSessionImpl baseDir sid = do
  mHandle <- readJsonFile (handlePath baseDir sid)
  case mHandle of
    Nothing -> pure False
    Just h -> do
      now <- getCurrentTime
      sessionIds <- handleIdsForState baseDir (handleStateId h)
      mapM_ (revokeHandle baseDir now) sessionIds
      pure True

handleIdsForState :: FilePath -> String -> IO [String]
handleIdsForState baseDir targetStateId = do
  entries <- listDirectory (baseDir </> "handles")
  fmap concat $ mapM (matchHandle targetStateId) entries
  where
    matchHandle stateId fileName =
      case stripJsonExt fileName of
        Nothing -> pure []
        Just sid -> do
          mHandle <- readJsonFile (handlePath baseDir sid)
          case mHandle of
            Just h | handleStateId h == stateId -> pure [sid]
            _ -> pure []

stripJsonExt :: String -> Maybe String
stripJsonExt fileName =
  if ".json" `isSuffixOf` fileName
    then Just (take (length fileName - 5) fileName)
    else Nothing

revokeHandle :: FilePath -> UTCTime -> String -> IO ()
revokeHandle baseDir now sid = do
  mHandle <- readJsonFile (handlePath baseDir sid)
  case mHandle of
    Nothing -> pure ()
    Just h -> writeJsonAtomic (handlePath baseDir sid) h { handleRevokedAt = Just now }


isStateValid :: UTCTime -> SessionState -> Bool
isStateValid now st =
  stateExpiresAt st > now && stateIdleExpiresAt st > now && not (isJust $ stateRevokedAt st)

touchState :: FilePath -> SessionConfig -> UTCTime -> SessionState -> IO ()
touchState baseDir SessionConfig {sessionIdleTtlSeconds} now st = do
  let newIdle = min (stateExpiresAt st) (addUTCTime sessionIdleTtlSeconds now)
  writeJsonAtomic (statePath baseDir $ stateId st) st { stateIdleExpiresAt = newIdle }

getOrCreateState :: FilePath -> SessionConfig -> String -> UTCTime -> IO SessionState
getOrCreateState baseDir config@SessionConfig {sessionAbsoluteTtlSeconds, sessionIdleTtlSeconds} userId now = do
  mBinding <- readJsonFile (userBindingPath baseDir userId)
  case mBinding of
    Just UserStateBinding {boundStateId} -> do
      mSt <- readJsonFile (statePath baseDir boundStateId)
      case mSt of
        Just st | isStateValid now st -> pure st
        _ -> createNewState
    _ -> createNewState
  where
    createNewState = do
      stId <- toString <$> nextRandom
      let st = SessionState
            { stateId = stId
            , stateUserId = userId
            , stateCreatedAt = now
            , stateExpiresAt = addUTCTime sessionAbsoluteTtlSeconds now
            , stateIdleExpiresAt = addUTCTime sessionIdleTtlSeconds now
            , stateRevokedAt = Nothing
            }
      writeJsonAtomic (statePath baseDir stId) st
      writeJsonAtomic (userBindingPath baseDir userId) UserStateBinding {boundStateId = stId}
      pure st

handlePath :: FilePath -> String -> FilePath
handlePath baseDir sid = baseDir </> "handles" </> sid ++ ".json"

statePath :: FilePath -> String -> FilePath
statePath baseDir stId = baseDir </> "states" </> stId ++ ".json"

userBindingPath :: FilePath -> String -> FilePath
userBindingPath baseDir userId = baseDir </> "users" </> userId ++ ".json"

readJsonFile :: FromJSON a => FilePath -> IO (Maybe a)
readJsonFile fp = do
  exists <- doesFileExist fp
  if not exists
    then pure Nothing
    else do
      content <- BL.readFile fp
      pure (decode content)

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
constantTimeEq a b = BA.constEq (BS8.pack a) (BS8.pack b)

digestFor :: String -> String -> String
digestFor secret sid = BS8.unpack $ convertToBase Base16 mac
  where
    mac :: HMAC SHA256
    mac = hmac (BS8.pack secret) (BS8.pack sid)
