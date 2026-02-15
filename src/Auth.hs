{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Auth (AuthRequest(..), AuthError(..), AuthRequestError(..), createUser, signinUser) where

import Prelude hiding (writeFile)
import Control.Monad (when, unless)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), encode, decode, (.:), withObject, object, (.=))
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.Char (isAlphaNum)
import Data.Password.Argon2 (PasswordHash(..), PasswordCheck(..), Argon2, mkPassword, hashPassword, checkPassword)
import qualified Data.Text as Text (Text, null, length)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectory, getCurrentDirectory, canonicalizePath, makeAbsolute, emptyPermissions, setOwnerReadable, setOwnerWritable, setOwnerSearchable, setPermissions)
import System.FilePath ((</>), normalise, takeDirectory, takeFileName, addTrailingPathSeparator)
import Data.List (isPrefixOf)
import Control.Exception (try, IOException)
import System.IO.Error (isAlreadyExistsError)
import qualified Data.ByteString.Lazy as BL

data AuthError = BadRequest !AuthRequestError | UserAlreadyExists | InvalidCredentials | TechnicalError !AuthTechnicalError
data AuthRequestError = EmptyUsername | EmptyPassword | UsernameDoesNotRespectPattern | UsernameTooShort | UsernameTooLong | PasswordTooShort
data AuthTechnicalError = UsersDirDoesNotExist | UserStorageFailure | UserReadFailure
type AuthAppM a = ExceptT AuthError IO a

data AuthRequest = AuthRequest { username :: !String, password :: !Text.Text }
  deriving Show
instance FromJSON AuthRequest where
  parseJSON = withObject "AuthData" $ \value -> AuthRequest
    <$> value .: "username"
    <*> value .: "password"

checkUsername :: String -> AuthAppM ()
checkUsername u = do
  when (null u) $ throwError $ BadRequest EmptyUsername
  when (length u < 3) $ throwError $ BadRequest UsernameTooShort
  when (length u > 32) $ throwError $ BadRequest UsernameTooLong
  unless (all isAllowedUsernameChar u) $ throwError $ BadRequest UsernameDoesNotRespectPattern
  where
    isAllowedUsernameChar c = isAlphaNum c || c `elem` ("._-" :: String)

checkPasswordRules :: Text.Text -> AuthAppM ()
checkPasswordRules p = do
  when (Text.null p) $ throwError $ BadRequest EmptyPassword
  when (Text.length p < 12) $ throwError $ BadRequest PasswordTooShort

createUser :: AuthRequest -> AuthAppM ()
createUser (AuthRequest {username, password}) = do
  checkUsername username
  checkPasswordRules password
  hashPass <- liftIO $ hashPassword $ mkPassword password
  persistUser username hashPass

data PersistedUser = PersistedUser {uname :: !String, passwordHash :: !(PasswordHash Argon2)}

ensureChild :: FilePath -> FilePath -> IO (Maybe FilePath)
ensureChild parent child = do
  parentCanonical <- canonicalizePath parent
  childAbsolute <- makeAbsolute (normalise child)
  childParentCanonical <- canonicalizePath (takeDirectory childAbsolute)
  let childCanonical = childParentCanonical </> takeFileName childAbsolute
      parentPrefix = addTrailingPathSeparator parentCanonical
  if childCanonical == parentCanonical || parentPrefix `isPrefixOf` childCanonical
    then pure (Just childCanonical)
    else pure Nothing
instance ToJSON PersistedUser where
  toJSON (PersistedUser {uname, passwordHash}) =
    object [ "uname" .= uname, "passwordHash" .= unPasswordHash passwordHash ]

instance FromJSON PersistedUser where
  parseJSON = withObject "PersistedUser" $ \value -> PersistedUser
    <$> value .: "uname"
    <*> (PasswordHash <$> value .: "passwordHash")

persistUser :: String -> PasswordHash Argon2 -> AuthAppM ()
persistUser username passwordHash = do
  cd <- liftIO getCurrentDirectory
  let usersDir = cd </> "data" </> "users"
      userDir = usersDir </> username
      profileFile = userDir </> "profile.json"
  safeUserDir <- liftIO $ ensureChild usersDir userDir
  safeProfile <- liftIO $ ensureChild usersDir profileFile
  dirExists <- liftIO (doesDirectoryExist usersDir)
  if not dirExists
    then throwError $ TechnicalError UsersDirDoesNotExist
    else case (safeUserDir, safeProfile) of
      (Just realUserDir, Just realProfile) -> do
        creationResult <- liftIO $ try (createDirectory realUserDir) :: AuthAppM (Either IOException ())
        case creationResult of
          Left ioErr ->
            if isAlreadyExistsError ioErr
              then throwError UserAlreadyExists
              else throwError $ TechnicalError UserStorageFailure
          Right _ -> do
            writeResult <- liftIO (try $ do
              setPermissions realUserDir (setOwnerSearchable True $ setOwnerWritable True $ setOwnerReadable True emptyPermissions)
              writeFile realProfile (encode $ PersistedUser {uname = username, passwordHash = passwordHash})
              setPermissions realProfile (setOwnerWritable True $ setOwnerReadable True emptyPermissions)) :: AuthAppM (Either IOException ())
            case writeResult of
              Left _ -> throwError $ TechnicalError UserStorageFailure
              Right _ -> pure ()
      _ -> throwError $ BadRequest UsernameDoesNotRespectPattern

signinUser :: AuthRequest -> AuthAppM ()
signinUser (AuthRequest {username, password}) = do
  cd <- liftIO getCurrentDirectory
  let usersDir = cd </> "data" </> "users"
      profileFile = usersDir </> username </> "profile.json"
  safeProfile <- liftIO $ ensureChild usersDir profileFile
  case safeProfile of
    Nothing -> throwError InvalidCredentials
    Just realProfile -> do
      profileExists <- liftIO $ doesFileExist realProfile
      if not profileExists
        then throwError InvalidCredentials
        else do
          maybeUser <- liftIO $ decode <$> BL.readFile realProfile
          case maybeUser of
            Nothing -> throwError $ TechnicalError UserReadFailure
            Just (PersistedUser {passwordHash}) -> do
              let checkResult = checkPassword (mkPassword password) passwordHash
              case checkResult of
                PasswordCheckSuccess -> pure ()
                PasswordCheckFail -> throwError InvalidCredentials
