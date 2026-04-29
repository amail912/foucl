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
  , postgresAuthRepository
  , verifyPostgresAuthStorage
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
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
import Data.Function ((&))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Password.Argon2 (Argon2, PasswordHash(..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Pool (Pool)
import Database.PostgreSQL.Simple
  ( Connection
  , SqlError(..)
  , Only(..)
  , execute
  , query
  , query_
  )
import Repository (RepositoryError(..))
import Helpers (tryExcept, withPoolExceptHandled, withResourceMHandled)
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
import Control.Monad (when)

data UserRole = AdminRole | MemberRole deriving (Eq, Show)

data ApprovalStatus = ApprovedStatus | PendingStatus deriving (Eq, Show)

data PersistedUser = PersistedUser
  { uname :: !String
  , passwordHash :: !(PasswordHash Argon2)
  , userRole :: !UserRole
  , approvalStatus :: !ApprovalStatus
  }

data AuthRepository = AuthRepository
  { repoCreateUser :: !(PersistedUser -> ExceptT RepositoryError IO ())
  , repoLoadUserByUsername :: !(String -> ExceptT RepositoryError IO PersistedUser)
  , repoUpdateUser :: !(PersistedUser -> ExceptT RepositoryError IO ())
  , repoDeleteUserByUsername :: !(String -> ExceptT RepositoryError IO ())
  , repoListUsers :: !(ExceptT RepositoryError IO [PersistedUser])
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

postgresAuthRepository :: Pool Connection -> AuthRepository
postgresAuthRepository pool = AuthRepository { repoCreateUser = pgCreateUser pool
                                             , repoLoadUserByUsername = pgLoadUserByUsername pool
                                             , repoUpdateUser = pgUpdateUser pool
                                             , repoDeleteUserByUsername = pgDeleteUserByUsername pool
                                             , repoListUsers = pgListUsers pool
                                             }

verifyPostgresAuthStorage :: Pool Connection -> ExceptT String IO ()
verifyPostgresAuthStorage pool =
  withResourceMHandled
    (\err -> "Unable to connect to Postgres: " ++ show err)
    pool
    (\conn -> do
      _ <- tryExcept (query_ conn "SELECT 1" :: IO [Only Int])
                     (\err -> "Postgres ping query failed: " ++ show err)
      _ <- tryExcept (query_ conn "SELECT username, password_hash, role::text, approved FROM auth_users LIMIT 0" :: IO [(String, Text, Text, Bool)])
                     (\err -> "Auth schema check failed: " ++ show err)
      enumResult <- tryExcept (query conn "SELECT EXISTS(SELECT 1 FROM pg_type WHERE typname = ?)" (Only ("auth_user_role" :: String)) :: IO [Only Bool])
             (\err -> "Auth enum check failed: " ++ show err)
      case enumResult of
        [Only True] -> pure ()
        _ -> throwError "Auth schema check failed: enum auth_user_role is missing"
    )

fsCreateUser :: PersistedUser -> ExceptT RepositoryError IO ()
fsCreateUser persistedUser@PersistedUser {uname = username} = do
  usersDir <- usersDirectory
  let userDir = usersDir </> username
      profileFile = userDir </> "profile.json"
  safeUserDir <- ensureChild usersDir userDir
  safeProfile <- ensureChild usersDir profileFile
  dirExists <- tryExcept (doesDirectoryExist usersDir) (const ReadFailure)
  if not dirExists
    then throwError StorageFailure
    else case (safeUserDir, safeProfile) of
      (Just realUserDir, Just realProfile) -> do
        _ <- tryExcept (createDirectory realUserDir) mapCreateDirectoryException
        _ <- tryExcept
          (do
            setPermissions realUserDir (setOwnerSearchable True $ setOwnerWritable True $ setOwnerReadable True emptyPermissions)
            BL.writeFile realProfile (encode persistedUser)
            setPermissions realProfile (setOwnerWritable True $ setOwnerReadable True emptyPermissions))
          (const WriteFailure)
        pure ()
      _ -> throwError StorageFailure

fsLoadUserByUsername :: String -> ExceptT RepositoryError IO PersistedUser
fsLoadUserByUsername username = do
  usersDir <- usersDirectory
  let profileFile = usersDir </> username </> "profile.json"
  safeProfile <- ensureChild usersDir profileFile
  safeProfile & maybe (throwError NotFound) (\realProfile -> do
    profileExists <- tryExcept (doesFileExist realProfile) (const ReadFailure)
    if not profileExists
      then throwError NotFound
      else do
        content <- tryExcept (BL.readFile realProfile) (const ReadFailure)
        maybe (throwError ReadFailure) pure (decode content))

fsUpdateUser :: PersistedUser -> ExceptT RepositoryError IO ()
fsUpdateUser persistedUser@PersistedUser {uname = username} = do
  usersDir <- usersDirectory
  let profileFile = usersDir </> username </> "profile.json"
  safeProfile <- ensureChild usersDir profileFile
  safeProfile & maybe (throwError StorageFailure) (\realProfile -> do
    profileExists <- tryExcept (doesFileExist realProfile) (const ReadFailure)
    if not profileExists
      then throwError NotFound
      else do
        _ <- tryExcept (BL.writeFile realProfile (encode persistedUser)) (const WriteFailure)
        pure ())

fsDeleteUserByUsername :: String -> ExceptT RepositoryError IO ()
fsDeleteUserByUsername username = do
  usersDir <- usersDirectory
  let userDir = usersDir </> username
  safeUserDir <- ensureChild usersDir userDir
  safeUserDir & maybe (throwError StorageFailure) (\realUserDir -> do
    exists <- tryExcept (doesDirectoryExist realUserDir) (const ReadFailure)
    if not exists
      then throwError NotFound
      else do
        _ <- tryExcept (removeDirectoryRecursive realUserDir) (const WriteFailure)
        pure ())

fsListUsers :: ExceptT RepositoryError IO [PersistedUser]
fsListUsers = do
  usersDir <- usersDirectory
  dirExists <- tryExcept (doesDirectoryExist usersDir) (const ReadFailure)
  if not dirExists
    then throwError StorageFailure
    else do
      usernames <- tryExcept (listDirectory usersDir) (const ReadFailure)
      loaded <- mapM loadMaybe usernames
      pure (foldr maybeCons [] loaded)
  where
    maybeCons Nothing acc = acc
    maybeCons (Just x) acc = x : acc

    loadMaybe username = do
      result <- tryExcept (runExceptT (fsLoadUserByUsername username)) (const ReadFailure)
      case result of
        Left NotFound -> pure Nothing
        Left err -> throwError err
        Right user -> pure (Just user)

usersDirectory :: ExceptT RepositoryError IO FilePath
usersDirectory = do
  cd <- tryExcept getCurrentDirectory (const StorageFailure)
  pure (cd </> "data" </> "users")

ensureChild :: FilePath -> FilePath -> ExceptT RepositoryError IO (Maybe FilePath)
ensureChild parent child = do
  parentCanonical <- tryExcept (canonicalizePath parent) (const StorageFailure)
  childAbsolute <- tryExcept (makeAbsolute (normalise child)) (const StorageFailure)
  childParentCanonical <- tryExcept (canonicalizePath (takeDirectory childAbsolute)) (const StorageFailure)
  let childCanonical = childParentCanonical </> takeFileName childAbsolute
      parentPrefix = addTrailingPathSeparator parentCanonical
  pure $ if childCanonical == parentCanonical || parentPrefix `isPrefixOf` childCanonical then Just childCanonical else Nothing

mapCreateDirectoryException :: Ex.SomeException -> RepositoryError
mapCreateDirectoryException ex =
  case Ex.fromException ex :: Maybe Ex.IOException of
    Just ioErr | isAlreadyExistsError ioErr -> AlreadyExists
    _ -> StorageFailure

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

pgCreateUser :: Pool Connection -> PersistedUser -> ExceptT RepositoryError IO ()
pgCreateUser pool persistedUser =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    let roleValue = userRoleToDb (userRole persistedUser)
        approvedValue = approvalStatusToDb (approvalStatus persistedUser)
    writeResult <- tryExcept
      (execute
        conn
        "INSERT INTO auth_users (username, password_hash, role, approved) VALUES (?, ?, ?::auth_user_role, ?)"
        (uname persistedUser, unPasswordHash (passwordHash persistedUser), roleValue, approvedValue))
        mapWriteException
    pure ()

pgLoadUserByUsername :: Pool Connection -> String -> ExceptT RepositoryError IO PersistedUser
pgLoadUserByUsername pool username =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    readResult <-tryExcept
      (query
        conn
        "SELECT username, password_hash, role::text, approved FROM auth_users WHERE username = ?"
        (Only username))
        mapReadException
    case readResult of
      [] -> throwError NotFound
      ((dbUsername, dbPasswordHash, dbRole, dbApproved):_) ->
        case dbRoleToUserRole dbRole of
          Nothing -> throwError ReadFailure
          Just role ->
            pure PersistedUser
              { uname = dbUsername
              , passwordHash = PasswordHash dbPasswordHash
              , userRole = role
              , approvalStatus = dbToApprovalStatus dbApproved
              }

pgUpdateUser :: Pool Connection -> PersistedUser -> ExceptT RepositoryError IO ()
pgUpdateUser pool persistedUser =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    let roleValue = userRoleToDb (userRole persistedUser)
        approvedValue = approvalStatusToDb (approvalStatus persistedUser)
    affected <- tryExcept
      (execute
        conn
        "UPDATE auth_users SET password_hash = ?, role = ?::auth_user_role, approved = ? WHERE username = ?"
        (unPasswordHash (passwordHash persistedUser), roleValue, approvedValue, uname persistedUser))
        mapWriteException
    when (affected == 0) (throwError NotFound)

pgDeleteUserByUsername :: Pool Connection -> String -> ExceptT RepositoryError IO ()
pgDeleteUserByUsername pool username =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    affected <-tryExcept
      (execute conn "DELETE FROM auth_users WHERE username = ?" (Only username))
      mapWriteException
    when (affected == 0) (throwError NotFound)

pgListUsers :: Pool Connection -> ExceptT RepositoryError IO [PersistedUser]
pgListUsers pool =
  withPoolExceptHandled (const StorageFailure) pool $ \conn -> do
    rows <- tryExcept
      (query_ conn "SELECT username, password_hash, role::text, approved FROM auth_users ORDER BY username" :: IO [(String, Text, Text, Bool)])
      mapReadException
    mapM decodeRow rows
  where
    decodeRow :: (String, Text, Text, Bool) -> ExceptT RepositoryError IO PersistedUser
    decodeRow (dbUsername, dbPasswordHash, dbRole, dbApproved) =
      case dbRoleToUserRole dbRole of
        Nothing -> throwError ReadFailure
        Just role ->
          pure PersistedUser
            { uname = dbUsername
            , passwordHash = PasswordHash dbPasswordHash
            , userRole = role
            , approvalStatus = dbToApprovalStatus dbApproved
            }

mapReadException :: Ex.SomeException -> RepositoryError
mapReadException ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr ->
      if isStorageSqlError sqlErr
        then StorageFailure
        else ReadFailure
    Nothing -> StorageFailure

mapWriteException :: Ex.SomeException -> RepositoryError
mapWriteException ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr
      | isUniqueViolation sqlErr -> AlreadyExists
      | isStorageSqlError sqlErr -> StorageFailure
      | otherwise -> WriteFailure
    Nothing -> StorageFailure

isUniqueViolation :: SqlError -> Bool
isUniqueViolation sqlErr = sqlState sqlErr == BS8.pack "23505"

isStorageSqlError :: SqlError -> Bool
isStorageSqlError sqlErr = "08" `BS8.isPrefixOf` sqlState sqlErr

userRoleToDb :: UserRole -> Text
userRoleToDb AdminRole = "admin"
userRoleToDb MemberRole = "member"

dbRoleToUserRole :: Text -> Maybe UserRole
dbRoleToUserRole "admin" = Just AdminRole
dbRoleToUserRole "member" = Just MemberRole
dbRoleToUserRole _ = Nothing

approvalStatusToDb :: ApprovalStatus -> Bool
approvalStatusToDb ApprovedStatus = True
approvalStatusToDb PendingStatus = False

dbToApprovalStatus :: Bool -> ApprovalStatus
dbToApprovalStatus True = ApprovedStatus
dbToApprovalStatus False = PendingStatus
