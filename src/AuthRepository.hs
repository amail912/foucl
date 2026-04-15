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
import qualified Data.ByteString.Char8 as BS8
import Data.Password.Argon2 (Argon2, PasswordHash(..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
  ( Connection
  , SqlError(..)
  , Only(..)
  , connectPostgreSQL
  , close
  , execute
  , query
  , query_
  )
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

postgresAuthRepository :: String -> AuthRepository
postgresAuthRepository connectionString =
  AuthRepository
    { repoCreateUser = pgCreateUser connectionString
    , repoLoadUserByUsername = pgLoadUserByUsername connectionString
    , repoUpdateUser = pgUpdateUser connectionString
    , repoDeleteUserByUsername = pgDeleteUserByUsername connectionString
    , repoListUsers = pgListUsers connectionString
    }

verifyPostgresAuthStorage :: String -> IO (Either String ())
verifyPostgresAuthStorage connectionString = do
  connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
  case connResult of
    Left err -> pure (Left ("Unable to connect to Postgres: " ++ show err))
    Right conn -> do
      pingResult <- Ex.try (query_ conn "SELECT 1" :: IO [Only Int]) :: IO (Either Ex.SomeException [Only Int])
      tableResult <- Ex.try (query_ conn "SELECT username, password_hash, role::text, approved FROM auth_users LIMIT 0" :: IO [(String, Text, Text, Bool)]) :: IO (Either Ex.SomeException [(String, Text, Text, Bool)])
      enumResult <- Ex.try (query conn "SELECT EXISTS(SELECT 1 FROM pg_type WHERE typname = ?)" (Only ("auth_user_role" :: String)) :: IO [Only Bool]) :: IO (Either Ex.SomeException [Only Bool])
      close conn
      case pingResult of
        Left err -> pure (Left ("Postgres ping query failed: " ++ show err))
        Right _ ->
          case tableResult of
            Left err -> pure (Left ("Auth schema check failed: " ++ show err))
            Right _ ->
              case enumResult of
                Left err -> pure (Left ("Auth enum check failed: " ++ show err))
                Right [Only True] -> pure (Right ())
                Right _ -> pure (Left "Auth schema check failed: enum auth_user_role is missing")

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

pgCreateUser :: String -> PersistedUser -> ExceptT RepositoryError IO ()
pgCreateUser connectionString persistedUser =
  withPgConnection connectionString StorageFailure $ \conn -> do
    let roleValue = userRoleToDb (userRole persistedUser)
        approvedValue = approvalStatusToDb (approvalStatus persistedUser)
    writeResult <- liftIO (Ex.try
      (execute
        conn
        "INSERT INTO auth_users (username, password_hash, role, approved) VALUES (?, ?, ?::auth_user_role, ?)"
        (uname persistedUser, unPasswordHash (passwordHash persistedUser), roleValue, approvedValue))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgLoadUserByUsername :: String -> String -> ExceptT RepositoryError IO PersistedUser
pgLoadUserByUsername connectionString username =
  withPgConnection connectionString StorageFailure $ \conn -> do
    readResult <- liftIO (Ex.try
      (query
        conn
        "SELECT username, password_hash, role::text, approved FROM auth_users WHERE username = ?"
        (Only username))
      :: IO (Either Ex.SomeException [(String, Text, Text, Bool)]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right [] -> throwError NotFound
      Right ((dbUsername, dbPasswordHash, dbRole, dbApproved):_) ->
        case dbRoleToUserRole dbRole of
          Nothing -> throwError ReadFailure
          Just role ->
            pure PersistedUser
              { uname = dbUsername
              , passwordHash = PasswordHash dbPasswordHash
              , userRole = role
              , approvalStatus = dbToApprovalStatus dbApproved
              }

pgUpdateUser :: String -> PersistedUser -> ExceptT RepositoryError IO ()
pgUpdateUser connectionString persistedUser =
  withPgConnection connectionString StorageFailure $ \conn -> do
    let roleValue = userRoleToDb (userRole persistedUser)
        approvedValue = approvalStatusToDb (approvalStatus persistedUser)
    writeResult <- liftIO (Ex.try
      (execute
        conn
        "UPDATE auth_users SET password_hash = ?, role = ?::auth_user_role, approved = ? WHERE username = ?"
        (unPasswordHash (passwordHash persistedUser), roleValue, approvedValue, uname persistedUser))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right affected -> when (affected == 0) (throwError NotFound)

pgDeleteUserByUsername :: String -> String -> ExceptT RepositoryError IO ()
pgDeleteUserByUsername connectionString username =
  withPgConnection connectionString StorageFailure $ \conn -> do
    writeResult <- liftIO (Ex.try
      (execute conn "DELETE FROM auth_users WHERE username = ?" (Only username))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right affected -> when (affected == 0) (throwError NotFound)

pgListUsers :: String -> ExceptT RepositoryError IO [PersistedUser]
pgListUsers connectionString =
  withPgConnection connectionString StorageFailure $ \conn -> do
    readResult <- liftIO (Ex.try
      (query_ conn "SELECT username, password_hash, role::text, approved FROM auth_users ORDER BY username" :: IO [(String, Text, Text, Bool)])
      :: IO (Either Ex.SomeException [(String, Text, Text, Bool)]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right rows -> mapM decodeRow rows
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

withPgConnection :: String -> RepositoryError -> (Connection -> ExceptT RepositoryError IO a) -> ExceptT RepositoryError IO a
withPgConnection connectionString connectionError action = do
  connResult <- liftIO $ Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: ExceptT RepositoryError IO (Either Ex.SomeException Connection)
  case connResult of
    Left _ -> throwError connectionError
    Right conn -> do
      runResult <- liftIO (runExceptT (action conn))
      _ <- liftIO $ Ex.try (close conn) :: ExceptT RepositoryError IO (Either Ex.SomeException ())
      either throwError pure runResult

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
