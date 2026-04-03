{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Auth
  ( AuthRequest(..)
  , AuthError(..)
  , AuthRequestError(..)
  , AuthenticatedProfile(..)
  , UserRole(..)
  , ApprovalStatus(..)
  , createUser
  , createUserWithBootstrapAdmin
  , loadAuthenticatedProfile
  , signinUser
  , userExists
  , isApprovedAdmin
  , listPendingUsers
  , approveUser
  , deletePendingUser
  ) where

import Prelude hiding (writeFile)
import Control.Monad (when, unless)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), encode, decode, (.:), (.:?), withObject, withText, object, (.=))
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.Char (isAlphaNum)
import Data.Password.Argon2 (PasswordHash(..), PasswordCheck(..), Argon2, mkPassword, hashPassword, checkPassword)
import qualified Data.Text as Text (Text, null, length)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectory, getCurrentDirectory, canonicalizePath, makeAbsolute, emptyPermissions, setOwnerReadable, setOwnerWritable, setOwnerSearchable, setPermissions, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>), normalise, takeDirectory, takeFileName, addTrailingPathSeparator)
import Data.List (isPrefixOf)
import Control.Exception (try, IOException)
import System.IO.Error (isAlreadyExistsError)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)

data AuthError = BadRequest !AuthRequestError | UserAlreadyExists | InvalidCredentials | AccountPendingApproval | ResourceNotFound | TechnicalError !AuthTechnicalError
data AuthRequestError = EmptyUsername | EmptyPassword | UsernameDoesNotRespectPattern | UsernameTooShort | UsernameTooLong | PasswordTooShort
data AuthTechnicalError = UsersDirDoesNotExist | UserStorageFailure | UserReadFailure deriving Eq
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

data AuthenticatedProfile = AuthenticatedProfile
  { authProfileUsername :: !String
  , authProfileRoles :: ![UserRole]
  , authProfileApproved :: !Bool
  } deriving (Eq, Show)

data UserRole = AdminRole | MemberRole deriving (Eq, Show)
data ApprovalStatus = ApprovedStatus | PendingStatus deriving (Eq, Show)

instance ToJSON AuthenticatedProfile where
  toJSON (AuthenticatedProfile username roles approved) =
    object
      [ "username" .= username
      , "roles" .= roles
      , "approved" .= approved
      ]

instance ToJSON UserRole where
  toJSON AdminRole = "admin"
  toJSON MemberRole = "member"

instance FromJSON UserRole where
  parseJSON = withText "UserRole" $ \case
    "admin" -> pure AdminRole
    "member" -> pure MemberRole
    _ -> fail "Invalid user role"

instance ToJSON ApprovalStatus where
  toJSON ApprovedStatus = "approved"
  toJSON PendingStatus = "pending"

instance FromJSON ApprovalStatus where
  parseJSON = withText "ApprovalStatus" $ \case
    "approved" -> pure ApprovedStatus
    "pending" -> pure PendingStatus
    _ -> fail "Invalid approval status"

createUser :: AuthRequest -> AuthAppM ()
createUser = createUserWithBootstrapAdmin Nothing

createUserWithBootstrapAdmin :: Maybe String -> AuthRequest -> AuthAppM ()
createUserWithBootstrapAdmin bootstrapAdminUsername (AuthRequest {username, password}) = do
  checkUsername username
  checkPasswordRules password
  hashPass <- liftIO $ hashPassword $ mkPassword password
  let (role, approvalStatus) =
        case bootstrapAdminUsername of
          Just adminUsername | username == adminUsername -> (AdminRole, ApprovedStatus)
          _ -> (MemberRole, PendingStatus)
  persistUser username hashPass role approvalStatus

data PersistedUser = PersistedUser
  { uname :: !String
  , passwordHash :: !(PasswordHash Argon2)
  , userRole :: !UserRole
  , approvalStatus :: !ApprovalStatus
  }

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
  toJSON (PersistedUser {uname, passwordHash, userRole, approvalStatus}) =
    object
      [ "uname" .= uname
      , "passwordHash" .= unPasswordHash passwordHash
      , "role" .= userRole
      , "approvalStatus" .= approvalStatus
      ]

instance FromJSON PersistedUser where
  parseJSON = withObject "PersistedUser" $ \value -> PersistedUser
    <$> value .: "uname"
    <*> (PasswordHash <$> value .: "passwordHash")
    <*> (value .:? "role" >>= maybe (pure MemberRole) pure)
    <*> (value .:? "approvalStatus" >>= maybe (pure ApprovedStatus) pure)

persistUser :: String -> PasswordHash Argon2 -> UserRole -> ApprovalStatus -> AuthAppM ()
persistUser username passwordHash userRole approvalStatus = do
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
              writeFile realProfile (encode $ PersistedUser {uname = username, passwordHash = passwordHash, userRole = userRole, approvalStatus = approvalStatus})
              setPermissions realProfile (setOwnerWritable True $ setOwnerReadable True emptyPermissions)) :: AuthAppM (Either IOException ())
            case writeResult of
              Left _ -> throwError $ TechnicalError UserStorageFailure
              Right _ -> pure ()
      _ -> throwError $ BadRequest UsernameDoesNotRespectPattern

signinUser :: AuthRequest -> AuthAppM AuthenticatedProfile
signinUser (AuthRequest {username, password}) = do
  maybeUser <- liftIO $ loadPersistedUser username
  case maybeUser of
    Left err -> throwError $ TechnicalError err
    Right Nothing -> throwError InvalidCredentials
    Right (Just persistedUser@(PersistedUser {passwordHash, approvalStatus})) -> do
      let checkResult = checkPassword (mkPassword password) passwordHash
      case checkResult of
        PasswordCheckFail -> throwError InvalidCredentials
        PasswordCheckSuccess ->
          case approvalStatus of
            PendingStatus -> throwError AccountPendingApproval
            ApprovedStatus -> pure (toAuthenticatedProfile persistedUser)

userExists :: String -> IO Bool
userExists username = do
  maybeUser <- loadPersistedUser username
  pure $ case maybeUser of
    Right (Just _) -> True
    _ -> False

isApprovedAdmin :: String -> IO (Either AuthTechnicalError Bool)
isApprovedAdmin username = do
  maybeUser <- loadPersistedUser username
  pure $ case maybeUser of
    Left err -> Left err
    Right Nothing -> Right False
    Right (Just PersistedUser {userRole, approvalStatus}) ->
      Right (userRole == AdminRole && approvalStatus == ApprovedStatus)

listPendingUsers :: IO (Either AuthTechnicalError [String])
listPendingUsers =
  fmap (map uname . filter isPending) <$> listPersistedUsers
  where
    isPending PersistedUser {approvalStatus} = approvalStatus == PendingStatus

approveUser :: String -> AuthAppM ()
approveUser username = do
  maybeUser <- liftIO $ loadPersistedUser username
  case maybeUser of
    Left err -> throwError $ TechnicalError err
    Right Nothing -> throwError InvalidCredentials
    Right (Just persistedUser) -> liftIO $ storePersistedUser persistedUser { approvalStatus = ApprovedStatus }

deletePendingUser :: String -> AuthAppM ()
deletePendingUser username = do
  maybeUser <- liftIO $ loadPersistedUser username
  case maybeUser of
    Left err -> throwError $ TechnicalError err
    Right Nothing -> throwError ResourceNotFound
    Right (Just PersistedUser {approvalStatus})
      | approvalStatus /= PendingStatus -> throwError ResourceNotFound
      | otherwise -> do
          deleteResult <- liftIO $ deletePersistedUser username
          case deleteResult of
            Left err -> throwError $ TechnicalError err
            Right () -> pure ()

loadAuthenticatedProfile :: String -> AuthAppM AuthenticatedProfile
loadAuthenticatedProfile username = do
  maybeUser <- liftIO $ loadPersistedUser username
  case maybeUser of
    Left err -> throwError $ TechnicalError err
    Right Nothing -> throwError $ TechnicalError UserReadFailure
    Right (Just persistedUser) -> pure (toAuthenticatedProfile persistedUser)

loadPersistedUser :: String -> IO (Either AuthTechnicalError (Maybe PersistedUser))
loadPersistedUser username = do
  usersDir <- usersDirectory
  let profileFile = usersDir </> username </> "profile.json"
  safeProfile <- ensureChild usersDir profileFile
  case safeProfile of
    Nothing -> pure (Right Nothing)
    Just realProfile -> do
      profileExists <- doesFileExist realProfile
      if not profileExists
        then pure (Right Nothing)
        else do
          maybeUser <- decode <$> BL.readFile realProfile
          pure $ case maybeUser of
            Nothing -> Left UserReadFailure
            Just user -> Right (Just user)

storePersistedUser :: PersistedUser -> IO ()
storePersistedUser persistedUser@PersistedUser {uname = username} = do
  usersDir <- usersDirectory
  let profileFile = usersDir </> username </> "profile.json"
  maybeSafeProfile <- ensureChild usersDir profileFile
  case maybeSafeProfile of
    Just realProfile -> writeFile realProfile (encode persistedUser)
    Nothing -> pure ()

deletePersistedUser :: String -> IO (Either AuthTechnicalError ())
deletePersistedUser username = do
  usersDir <- usersDirectory
  let userDir = usersDir </> username
  maybeSafeUserDir <- ensureChild usersDir userDir
  case maybeSafeUserDir of
    Nothing -> pure (Left UserStorageFailure)
    Just realUserDir -> do
      deleteResult <- try (removeDirectoryRecursive realUserDir) :: IO (Either IOException ())
      pure $
        case deleteResult of
          Left _ -> Left UserStorageFailure
          Right () -> Right ()

listPersistedUsers :: IO (Either AuthTechnicalError [PersistedUser])
listPersistedUsers = do
  usersDir <- usersDirectory
  dirExists <- doesDirectoryExist usersDir
  if not dirExists
    then pure $ Left UsersDirDoesNotExist
    else do
      usernames <- listDirectory usersDir
      users <- mapM loadPersistedUser usernames
      pure $ sequenceUsers users
  where
    sequenceUsers entries =
      case sequence entries of
        Left err -> Left err
        Right loaded -> Right (catMaybes loaded)

usersDirectory :: IO FilePath
usersDirectory = do
  cd <- getCurrentDirectory
  pure (cd </> "data" </> "users")

toAuthenticatedProfile :: PersistedUser -> AuthenticatedProfile
toAuthenticatedProfile PersistedUser {uname, userRole, approvalStatus} =
  AuthenticatedProfile
    { authProfileUsername = uname
    , authProfileRoles = [userRole]
    , authProfileApproved = approvalStatus == ApprovedStatus
    }
