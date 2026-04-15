{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AuthRepository
  ( AuthRepository(..)
  , PersistedUser(..)
  , UserRole(..)
  , ApprovalStatus(..)
  , defaultAuthRepository
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , decode
  , encode
  , object
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.ByteString.Lazy as BL
import Data.Password.Argon2 (Argon2, PasswordHash(..))
import Repository (RepositoryError(..))
import System.Directory
  ( canonicalizePath
  , createDirectory
  , doesDirectoryExist
  , doesFileExist
  , emptyPermissions
  , getCurrentDirectory
  , listDirectory
  , makeAbsolute
  , removeDirectoryRecursive
  , setOwnerReadable
  , setOwnerSearchable
  , setOwnerWritable
  , setPermissions
  )
import System.FilePath ((</>), addTrailingPathSeparator, normalise, takeDirectory, takeFileName)
import System.IO.Error (isAlreadyExistsError)
import qualified Control.Exception as Ex

data UserRole = AdminRole | MemberRole deriving (Eq, Show)

data ApprovalStatus = ApprovedStatus | PendingStatus deriving (Eq, Show)

data PersistedUser = PersistedUser
  { uname :: !String
  , passwordHash :: !(PasswordHash Argon2)
  , userRole :: !UserRole
  , approvalStatus :: !ApprovalStatus
  }

data AuthRepository = AuthRepository
  { repoCreateUser :: PersistedUser -> ExceptT RepositoryError IO ()
  , repoLoadUserByUsername :: String -> ExceptT RepositoryError IO PersistedUser
  , repoUpdateUser :: PersistedUser -> ExceptT RepositoryError IO ()
  , repoDeleteUserByUsername :: String -> ExceptT RepositoryError IO ()
  , repoListUsers :: ExceptT RepositoryError IO [PersistedUser]
  }

instance ToJSON PersistedUser where
  toJSON PersistedUser {uname, passwordHash, userRole, approvalStatus} =
    object
      [ "uname" .= uname
      , "passwordHash" .= unPasswordHash passwordHash
      , "role" .= userRole
      , "approvalStatus" .= approvalStatus
      ]

instance FromJSON PersistedUser where
  parseJSON = withObject "PersistedUser" $ \value ->
    PersistedUser
      <$> value .: "uname"
      <*> (PasswordHash <$> value .: "passwordHash")
      <*> (value .:? "role" >>= maybe (pure MemberRole) pure)
      <*> (value .:? "approvalStatus" >>= maybe (pure ApprovedStatus) pure)

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

defaultAuthRepository :: AuthRepository
defaultAuthRepository =
  AuthRepository
    { repoCreateUser = fsCreateUser
    , repoLoadUserByUsername = fsLoadUserByUsername
    , repoUpdateUser = fsUpdateUser
    , repoDeleteUserByUsername = fsDeleteUserByUsername
    , repoListUsers = fsListUsers
    }

fsCreateUser :: PersistedUser -> ExceptT RepositoryError IO ()
fsCreateUser persistedUser@PersistedUser {uname = username} = do
  usersDir <- usersDirectory
  let userDir = usersDir </> username
      profileFile = userDir </> "profile.json"
  safeUserDir <- ensureChild usersDir userDir
  safeProfile <- ensureChild usersDir profileFile
  dirExists <- ioOr ReadFailure (doesDirectoryExist usersDir)
  if not dirExists
    then throwError StorageFailure
    else case (safeUserDir, safeProfile) of
      (Just realUserDir, Just realProfile) -> do
        creationResult <- ioOr StorageFailure (Ex.try (createDirectory realUserDir) :: IO (Either Ex.IOException ()))
        case creationResult of
          Left ioErr
            | isAlreadyExistsError ioErr -> throwError AlreadyExists
            | otherwise -> throwError StorageFailure
          Right () -> do
            writeResult <- ioOr WriteFailure $ Ex.try $ do
              setPermissions realUserDir (setOwnerSearchable True $ setOwnerWritable True $ setOwnerReadable True emptyPermissions)
              BL.writeFile realProfile (encode persistedUser)
              setPermissions realProfile (setOwnerWritable True $ setOwnerReadable True emptyPermissions)
            case (writeResult :: Either Ex.IOException ()) of
              Left _ -> throwError WriteFailure
              Right () -> pure ()
      _ -> throwError StorageFailure

fsLoadUserByUsername :: String -> ExceptT RepositoryError IO PersistedUser
fsLoadUserByUsername username = do
  usersDir <- usersDirectory
  let profileFile = usersDir </> username </> "profile.json"
  safeProfile <- ensureChild usersDir profileFile
  case safeProfile of
    Nothing -> throwError NotFound
    Just realProfile -> do
      profileExists <- ioOr ReadFailure (doesFileExist realProfile)
      if not profileExists
        then throwError NotFound
        else do
          content <- ioOr ReadFailure (BL.readFile realProfile)
          case decode content of
            Nothing -> throwError ReadFailure
            Just user -> pure user

fsUpdateUser :: PersistedUser -> ExceptT RepositoryError IO ()
fsUpdateUser persistedUser@PersistedUser {uname = username} = do
  usersDir <- usersDirectory
  let profileFile = usersDir </> username </> "profile.json"
  safeProfile <- ensureChild usersDir profileFile
  case safeProfile of
    Nothing -> throwError StorageFailure
    Just realProfile -> do
      profileExists <- ioOr ReadFailure (doesFileExist realProfile)
      if not profileExists
        then throwError NotFound
        else do
          writeRes <- ioOr WriteFailure (Ex.try (BL.writeFile realProfile (encode persistedUser)) :: IO (Either Ex.IOException ()))
          case writeRes of
            Left _ -> throwError WriteFailure
            Right () -> pure ()

fsDeleteUserByUsername :: String -> ExceptT RepositoryError IO ()
fsDeleteUserByUsername username = do
  usersDir <- usersDirectory
  let userDir = usersDir </> username
  safeUserDir <- ensureChild usersDir userDir
  case safeUserDir of
    Nothing -> throwError StorageFailure
    Just realUserDir -> do
      exists <- ioOr ReadFailure (doesDirectoryExist realUserDir)
      if not exists
        then throwError NotFound
        else do
          deleteResult <- ioOr WriteFailure (Ex.try (removeDirectoryRecursive realUserDir) :: IO (Either Ex.IOException ()))
          case deleteResult of
            Left _ -> throwError WriteFailure
            Right () -> pure ()

fsListUsers :: ExceptT RepositoryError IO [PersistedUser]
fsListUsers = do
  usersDir <- usersDirectory
  dirExists <- ioOr ReadFailure (doesDirectoryExist usersDir)
  if not dirExists
    then throwError StorageFailure
    else do
      usernames <- ioOr ReadFailure (listDirectory usersDir)
      loaded <- mapM loadMaybe usernames
      pure (foldr maybeCons [] loaded)
  where
    maybeCons Nothing acc = acc
    maybeCons (Just x) acc = x : acc

    loadMaybe username = do
      result <- ioOr ReadFailure (runExceptT (fsLoadUserByUsername username))
      case result of
        Left NotFound -> pure Nothing
        Left err -> throwError err
        Right user -> pure (Just user)

usersDirectory :: ExceptT RepositoryError IO FilePath
usersDirectory = do
  cd <- ioOr StorageFailure getCurrentDirectory
  pure (cd </> "data" </> "users")

ensureChild :: FilePath -> FilePath -> ExceptT RepositoryError IO (Maybe FilePath)
ensureChild parent child = do
  parentCanonical <- ioOr StorageFailure (canonicalizePath parent)
  childAbsolute <- ioOr StorageFailure (makeAbsolute (normalise child))
  childParentCanonical <- ioOr StorageFailure (canonicalizePath (takeDirectory childAbsolute))
  let childCanonical = childParentCanonical </> takeFileName childAbsolute
      parentPrefix = addTrailingPathSeparator parentCanonical
  pure $ if childCanonical == parentCanonical || parentPrefix `isPrefixOf` childCanonical then Just childCanonical else Nothing

ioOr :: RepositoryError -> IO a -> ExceptT RepositoryError IO a
ioOr err action = do
  result <- liftIO $ Ex.try action
  case result of
    Left (_ :: Ex.IOException) -> throwError err
    Right value -> pure value

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
