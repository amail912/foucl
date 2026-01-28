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
import Data.List (isPrefixOf)
import Data.Password.Argon2 (Password, PasswordHash(..), Argon2, mkPassword, hashPassword, checkPassword)
import qualified Data.Text as Text (Text, null)
import System.Directory (doesFileExist, doesDirectoryExist, getCurrentDirectory, canonicalizePath)
import System.FilePath ((</>), pathSeparator)

data SignupError = BadRequest !SignupRequestError | UserAlreadyExists | TechnicalError !SignupTechnicalError
data SignupRequestError = EmptyUsername | EmptyPassword | UsernameDoesNotRespectPattern
data SignupTechnicalError = UsersDirDoesNotExist
type SignupAppM a = ExceptT SignupError IO a

data SignupRequest = SignupRequest { username :: !String, password :: !Text.Text }
  deriving Show
instance FromJSON SignupRequest where
  parseJSON = withObject "SignupData" $ \value -> SignupRequest
    <$> value .: "username"
    <*> value .: "password"

checkUsername :: String -> SignupAppM ()
checkUsername u = when (null u) $ throwError $ BadRequest EmptyUsername

checkPasswordRules :: Text.Text -> SignupAppM ()
checkPasswordRules p = do
  when (Text.null p) $ throwError $ BadRequest EmptyPassword

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

ensureChild :: FilePath -> FilePath -> IO (Maybe FilePath)
ensureChild parent child = do
  isUnderParent <- isPrefixOf <$> canonicalizePath parent <*> canonicalizePath child
  if isUnderParent then pure (Just child) else pure Nothing

persistUser :: String -> PasswordHash Argon2 -> SignupAppM ()
persistUser username passwordHash = do
  cd <- liftIO getCurrentDirectory
  let filePath = cd </> "data" </> "users" </> username
      dataDir = cd </> ("data" <> [pathSeparator])
  userPath <- liftIO $ ensureChild dataDir filePath
  maybe (throwError $ BadRequest UsernameDoesNotRespectPattern)
        writeUserFile
        userPath
  where writeUserFile p = do
          dirExists <- liftIO (doesDirectoryExist $ "data" </> "users")
          if not dirExists
            then throwError $ TechnicalError UsersDirDoesNotExist
            else do
              exists <- liftIO $ doesFileExist ("data" </> "users" </> username)
              if not exists
                then throwError UserAlreadyExists
                else liftIO $ writeFile p (encode $ PersistedUser {uname = username, passwordHash = passwordHash})
