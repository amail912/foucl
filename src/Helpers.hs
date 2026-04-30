module Helpers
  ( tryExcept
  , withResourceM
  , withResourceMHandled
  , withPoolExceptHandled
  , mapSqlReadException
  , mapSqlWriteException
  , mapSqlWriteExceptionNoConflict
  , isStorageSqlError
  , isUniqueViolation
  ) where

import qualified Control.Exception as Ex
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.Pool (Pool, withResource)
import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple (SqlError(..))
import Repository (RepositoryError(..))

tryExcept :: IO a -> (Ex.SomeException -> e) -> ExceptT e IO a
tryExcept action onErr = do
  result <- lift (Ex.try action)
  either (throwE . onErr) pure result

withResourceM :: Pool a -> (a -> ExceptT e IO b) -> ExceptT e IO b
withResourceM pool action = ExceptT $ withResource pool (runExceptT . action)

withResourceMHandled :: (Ex.SomeException -> e) -> Pool a -> (a -> ExceptT e IO b) -> ExceptT e IO b
withResourceMHandled toErr pool action = ExceptT $ do
  result <- Ex.try (withResource pool (runExceptT . action))
  pure $ case result of
    Left ex -> Left (toErr ex)
    Right inner -> inner

withPoolExceptHandled :: (Ex.SomeException -> e) -> Pool a -> (a -> ExceptT e IO b) -> ExceptT e IO b
withPoolExceptHandled = withResourceMHandled

mapSqlReadException :: Ex.SomeException -> RepositoryError
mapSqlReadException ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr ->
      if isStorageSqlError sqlErr
        then StorageFailure
        else ReadFailure
    Nothing -> StorageFailure

mapSqlWriteException :: Ex.SomeException -> RepositoryError
mapSqlWriteException ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr
      | isUniqueViolation sqlErr -> AlreadyExists
      | isStorageSqlError sqlErr -> StorageFailure
      | otherwise -> WriteFailure
    Nothing -> StorageFailure

mapSqlWriteExceptionNoConflict :: Ex.SomeException -> RepositoryError
mapSqlWriteExceptionNoConflict ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr
      | isStorageSqlError sqlErr -> StorageFailure
      | otherwise -> WriteFailure
    Nothing -> StorageFailure

isUniqueViolation :: SqlError -> Bool
isUniqueViolation sqlErr = sqlState sqlErr == BS8.pack "23505"

isStorageSqlError :: SqlError -> Bool
isStorageSqlError sqlErr = BS8.pack "08" `BS8.isPrefixOf` sqlState sqlErr
