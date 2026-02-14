{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Auth (SignupRequest(..), SignupError(..), SignupRequestError(..), createUser) where

import Prelude hiding (writeFile)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), decode, encode, (.:), withObject, object, (.=))
import Data.ByteString.Lazy.Char8 (ByteString, writeFile)
import Data.Char (isAlphaNum)
import Data.Password.Argon2 (Password, PasswordHash(..), Argon2, mkPassword, hashPassword, checkPassword)
import qualified Data.Text as Text (Text, null, length)
import System.Directory (doesDirectoryExist, createDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import Control.Exception (try, IOException)

data SignupError = BadRequest !SignupRequestError | UserAlreadyExists | TechnicalError !SignupTechnicalError
data SignupRequestError = EmptyUsername | EmptyPassword | UsernameDoesNotRespectPattern | UsernameTooShort | UsernameTooLong | PasswordTooShort
data SignupTechnicalError = UsersDirDoesNotExist
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
instance ToJSON PersistedUser where
  toJSON (PersistedUser {uname, passwordHash}) =
    object [ "uname" .= uname, "passwordHash" .= unPasswordHash passwordHash ]

persistUser :: String -> PasswordHash Argon2 -> SignupAppM ()
persistUser username passwordHash = do
  cd <- liftIO getCurrentDirectory
  let usersDir = cd </> "data" </> "users"
      userDir = usersDir </> username
      profileFile = userDir </> "profile.json"
  dirExists <- liftIO (doesDirectoryExist usersDir)
  if not dirExists
    then throwError $ TechnicalError UsersDirDoesNotExist
    else do
      creationResult <- liftIO $ try (createDirectory userDir) :: SignupAppM (Either IOException ())
      case creationResult of
        Left _ -> throwError UserAlreadyExists
        Right _ -> liftIO $ writeFile profileFile (encode $ PersistedUser {uname = username, passwordHash = passwordHash})
