{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Auth (SignupRequest(..), SignupError(..), SignupRequestError(..), createUser) where

import Prelude hiding (writeFile)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), encode, (.:), withObject, object, (.=))
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.Char (isAlphaNum)
import Data.Password.Argon2 (PasswordHash(..), Argon2, mkPassword, hashPassword)
import qualified Data.Text as Text (Text, null, length)
import System.Directory (doesDirectoryExist, createDirectory, getCurrentDirectory, canonicalizePath, makeAbsolute, emptyPermissions, setOwnerReadable, setOwnerWritable, setOwnerSearchable, setPermissions)
import System.FilePath ((</>), normalise, takeDirectory, takeFileName, addTrailingPathSeparator)
import Data.List (isPrefixOf)
import Control.Exception (try, IOException)
import System.IO.Error (isAlreadyExistsError)

data SignupError = BadRequest !SignupRequestError | UserAlreadyExists | TechnicalError !SignupTechnicalError
data SignupRequestError = EmptyUsername | EmptyPassword | UsernameDoesNotRespectPattern | UsernameTooShort | UsernameTooLong | PasswordTooShort
data SignupTechnicalError = UsersDirDoesNotExist | UserStorageFailure
type SignupAppM a = ExceptT SignupError IO a

data SignupRequest = SignupRequest { username :: !String, password :: !Text.Text }
  deriving Show
instance FromJSON SignupRequest where
  parseJSON = withObject "SignupData" $ \value -> SignupRequest
    <$> value .: "username"
    <*> value .: "password"

checkUsername :: String -> SignupAppM ()
checkUsername u = do
  when (null u) $ throwError $ BadRequest EmptyUsername
  when (length u < 3) $ throwError $ BadRequest UsernameTooShort
  when (length u > 32) $ throwError $ BadRequest UsernameTooLong
  when (not $ all isAllowedUsernameChar u) $ throwError $ BadRequest UsernameDoesNotRespectPattern
  where
    isAllowedUsernameChar c = isAlphaNum c || c `elem` ("._-" :: String)

checkPasswordRules :: Text.Text -> SignupAppM ()
checkPasswordRules p = do
  when (Text.null p) $ throwError $ BadRequest EmptyPassword
  when (Text.length p < 12) $ throwError $ BadRequest PasswordTooShort

createUser :: SignupRequest -> SignupAppM ()
createUser (SignupRequest {username, password}) = do
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

persistUser :: String -> PasswordHash Argon2 -> SignupAppM ()
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
        creationResult <- liftIO $ try (createDirectory realUserDir) :: SignupAppM (Either IOException ())
        case creationResult of
          Left ioErr ->
            if isAlreadyExistsError ioErr
              then throwError UserAlreadyExists
              else throwError $ TechnicalError UserStorageFailure
          Right _ -> do
            writeResult <- liftIO (try $ do
              setPermissions realUserDir (setOwnerSearchable True $ setOwnerWritable True $ setOwnerReadable True emptyPermissions)
              writeFile realProfile (encode $ PersistedUser {uname = username, passwordHash = passwordHash})
              setPermissions realProfile (setOwnerWritable True $ setOwnerReadable True emptyPermissions)) :: SignupAppM (Either IOException ())
            case writeResult of
              Left _ -> throwError $ TechnicalError UserStorageFailure
              Right _ -> pure ()
      _ -> throwError $ BadRequest UsernameDoesNotRespectPattern
