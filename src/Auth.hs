{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( AuthRequest(..)
  , AuthError(..)
  , AuthRequestError(..)
  , AuthenticatedProfile(..)
  , UserRole(..)
  , ApprovalStatus(..)
  , AuthRepository
  , defaultAuthRepository
  , createUser
  , createUserWithBootstrapAdmin
  , loadAuthenticatedProfile
  , signinUser
  , userExists
  , isApprovedAdmin
  , listPendingUsers
  , listApprovedUsers
  , approveUser
  , deletePendingUser
  , deleteApprovedUser
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , (.:)
  , (.=)
  , object
  , withObject
  )
import Data.Char (isAlphaNum)
import Data.Password.Argon2
  ( Argon2
  , PasswordCheck(..)
  , PasswordHash
  , checkPassword
  , hashPassword
  , mkPassword
  )
import qualified Data.Text as Text (Text, length, null)
import AuthRepository
  ( ApprovalStatus(..)
  , AuthRepository(..)
  , PersistedUser(..)
  , UserRole(..)
  , defaultAuthRepository
  )
import Repository (RepositoryError(..))

data AuthError = BadRequest !AuthRequestError | UserAlreadyExists | InvalidCredentials | AccountPendingApproval | ResourceNotFound | ResourceConflict !String | TechnicalError !AuthTechnicalError

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
  }
  deriving (Eq, Show)

instance ToJSON AuthenticatedProfile where
  toJSON (AuthenticatedProfile profileUsername roles approved) =
    object
      [ "username" .= profileUsername
      , "roles" .= roles
      , "approved" .= approved
      ]

createUser :: AuthRepository -> AuthRequest -> AuthAppM ()
createUser authRepo = createUserWithBootstrapAdmin authRepo Nothing

createUserWithBootstrapAdmin :: AuthRepository -> Maybe String -> AuthRequest -> AuthAppM ()
createUserWithBootstrapAdmin authRepo bootstrapAdminUsername (AuthRequest {username, password}) = do
  checkUsername username
  checkPasswordRules password
  passwordHash <- liftIO $ hashPassword (mkPassword password)
  let (role, approvalStatus) =
        case bootstrapAdminUsername of
          Just adminUsername | username == adminUsername -> (AdminRole, ApprovedStatus)
          _ -> (MemberRole, PendingStatus)
      persistedUser =
        PersistedUser
          { uname = username
          , passwordHash
          , userRole = role
          , approvalStatus
          }
  createResult <- liftIO $ runExceptT (repoCreateUser authRepo persistedUser)
  case createResult of
    Left AlreadyExists -> throwError UserAlreadyExists
    Left StorageFailure -> throwError $ TechnicalError UsersDirDoesNotExist
    Left _ -> throwError $ TechnicalError UserStorageFailure
    Right () -> pure ()

signinUser :: AuthRepository -> AuthRequest -> AuthAppM AuthenticatedProfile
signinUser authRepo (AuthRequest {username, password}) = do
  userResult <- liftIO $ runExceptT (repoLoadUserByUsername authRepo username)
  case userResult of
    Left NotFound -> throwError InvalidCredentials
    Left ReadFailure -> throwError $ TechnicalError UserReadFailure
    Left _ -> throwError $ TechnicalError UserStorageFailure
    Right persistedUser@PersistedUser {passwordHash, approvalStatus} ->
      case checkPassword (mkPassword password) passwordHash of
        PasswordCheckFail -> throwError InvalidCredentials
        PasswordCheckSuccess ->
          case approvalStatus of
            PendingStatus -> throwError AccountPendingApproval
            ApprovedStatus -> pure (toAuthenticatedProfile persistedUser)

userExists :: AuthRepository -> String -> IO Bool
userExists authRepo username = do
  userResult <- runExceptT (repoLoadUserByUsername authRepo username)
  pure $ case userResult of
    Right _ -> True
    _ -> False

isApprovedAdmin :: AuthRepository -> String -> IO (Either AuthTechnicalError Bool)
isApprovedAdmin authRepo username = do
  userResult <- runExceptT (repoLoadUserByUsername authRepo username)
  pure $ case userResult of
    Left NotFound -> Right False
    Left ReadFailure -> Left UserReadFailure
    Left _ -> Left UserStorageFailure
    Right PersistedUser {userRole, approvalStatus} ->
      Right (userRole == AdminRole && approvalStatus == ApprovedStatus)

listPendingUsers :: AuthRepository -> IO (Either AuthTechnicalError [String])
listPendingUsers authRepo = do
  usersResult <- runExceptT (repoListUsers authRepo)
  pure $ case usersResult of
    Left ReadFailure -> Left UserReadFailure
    Left StorageFailure -> Left UsersDirDoesNotExist
    Left _ -> Left UserStorageFailure
    Right users -> Right (map uname (filter isPending users))
  where
    isPending PersistedUser {approvalStatus} = approvalStatus == PendingStatus

listApprovedUsers :: AuthRepository -> IO (Either AuthTechnicalError [AuthenticatedProfile])
listApprovedUsers authRepo = do
  usersResult <- runExceptT (repoListUsers authRepo)
  pure $ case usersResult of
    Left ReadFailure -> Left UserReadFailure
    Left StorageFailure -> Left UsersDirDoesNotExist
    Left _ -> Left UserStorageFailure
    Right users -> Right (map toAuthenticatedProfile (filter isApproved users))
  where
    isApproved PersistedUser {approvalStatus} = approvalStatus == ApprovedStatus

approveUser :: AuthRepository -> String -> AuthAppM ()
approveUser authRepo username = do
  userResult <- liftIO $ runExceptT (repoLoadUserByUsername authRepo username)
  case userResult of
    Left NotFound -> throwError ResourceNotFound
    Left ReadFailure -> throwError $ TechnicalError UserReadFailure
    Left _ -> throwError $ TechnicalError UserStorageFailure
    Right persistedUser -> do
      updateResult <- liftIO $ runExceptT (repoUpdateUser authRepo persistedUser {approvalStatus = ApprovedStatus})
      case updateResult of
        Left NotFound -> throwError ResourceNotFound
        Left _ -> throwError $ TechnicalError UserStorageFailure
        Right () -> pure ()

deletePendingUser :: AuthRepository -> String -> AuthAppM ()
deletePendingUser authRepo username = do
  userResult <- liftIO $ runExceptT (repoLoadUserByUsername authRepo username)
  case userResult of
    Left NotFound -> throwError ResourceNotFound
    Left ReadFailure -> throwError $ TechnicalError UserReadFailure
    Left _ -> throwError $ TechnicalError UserStorageFailure
    Right PersistedUser {approvalStatus}
      | approvalStatus /= PendingStatus -> throwError ResourceNotFound
      | otherwise -> do
          deleteResult <- liftIO $ runExceptT (repoDeleteUserByUsername authRepo username)
          case deleteResult of
            Left NotFound -> throwError ResourceNotFound
            Left _ -> throwError $ TechnicalError UserStorageFailure
            Right () -> pure ()

deleteApprovedUser :: AuthRepository -> String -> String -> String -> AuthAppM ()
deleteApprovedUser authRepo bootstrapAdminUsername currentUsername targetUsername
  | targetUsername == bootstrapAdminUsername = throwError $ ResourceConflict "Cannot delete bootstrap admin"
  | targetUsername == currentUsername = throwError $ ResourceConflict "Cannot delete your own account"
  | otherwise = do
      userResult <- liftIO $ runExceptT (repoLoadUserByUsername authRepo targetUsername)
      case userResult of
        Left NotFound -> throwError ResourceNotFound
        Left ReadFailure -> throwError $ TechnicalError UserReadFailure
        Left _ -> throwError $ TechnicalError UserStorageFailure
        Right PersistedUser {approvalStatus}
          | approvalStatus /= ApprovedStatus -> throwError ResourceNotFound
          | otherwise -> do
              deleteResult <- liftIO $ runExceptT (repoDeleteUserByUsername authRepo targetUsername)
              case deleteResult of
                Left NotFound -> throwError ResourceNotFound
                Left _ -> throwError $ TechnicalError UserStorageFailure
                Right () -> pure ()

loadAuthenticatedProfile :: AuthRepository -> String -> AuthAppM AuthenticatedProfile
loadAuthenticatedProfile authRepo username = do
  userResult <- liftIO $ runExceptT (repoLoadUserByUsername authRepo username)
  case userResult of
    Left ReadFailure -> throwError $ TechnicalError UserReadFailure
    Left NotFound -> throwError $ TechnicalError UserReadFailure
    Left _ -> throwError $ TechnicalError UserStorageFailure
    Right persistedUser -> pure (toAuthenticatedProfile persistedUser)

toAuthenticatedProfile :: PersistedUser -> AuthenticatedProfile
toAuthenticatedProfile PersistedUser {uname, userRole, approvalStatus} =
  AuthenticatedProfile
    { authProfileUsername = uname
    , authProfileRoles = [userRole]
    , authProfileApproved = approvalStatus == ApprovedStatus
    }
