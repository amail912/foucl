{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module NotesChecklistRepository
  ( NotesChecklistRepository(..)
  , NoteRepository
  , ChecklistRepository
  , defaultNoteRepository
  , defaultChecklistRepository
  , filesystemNoteRepository
  , filesystemChecklistRepository
  , postgresNoteRepository
  , postgresChecklistRepository
  , verifyPostgresNoteStorage
  , verifyPostgresChecklistStorage
  ) where

import Prelude hiding (id)
import ChecklistCrud (ChecklistServiceConfig, defaultChecklistServiceConfig)
import Control.Monad (foldM, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Crud
  ( CrudModificationException(..)
  , CrudReadException(..)
  , CrudWriteException(..)
  )
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int (Int64)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , Query
  , SqlError(..)
  , execute
  , query
  , query_
  )
import CrudStorage (createItem, deleteItem, getAllItems, modifyItem)
import Model (ChecklistContent, Content, Identifiable(..), NoteContent, StorageId(..), hash)
import NoteCrud (NoteServiceConfig, defaultNoteServiceConfig)
import System.IO (hFlush, stdout)
import SqlTiming (timedTry)
import Text.Printf (printf)
import qualified Control.Exception as Ex

data NotesChecklistRepository a = NotesChecklistRepository
  { repoCreateItem :: a -> ExceptT CrudWriteException IO StorageId
  , repoListItems :: ExceptT CrudReadException IO [Identifiable a]
  , repoDeleteItemById :: String -> ExceptT CrudWriteException IO ()
  , repoUpdateItem :: Identifiable a -> ExceptT CrudModificationException IO StorageId
  }

type NoteRepository = NotesChecklistRepository NoteContent

type ChecklistRepository = NotesChecklistRepository ChecklistContent

defaultNoteRepository :: NoteRepository
defaultNoteRepository = filesystemNoteRepository defaultNoteServiceConfig

defaultChecklistRepository :: ChecklistRepository
defaultChecklistRepository = filesystemChecklistRepository defaultChecklistServiceConfig

filesystemNoteRepository :: NoteServiceConfig -> NoteRepository
filesystemNoteRepository config =
  NotesChecklistRepository
    { repoCreateItem = createItem config
    , repoListItems = listItemsIgnoringParsingFailures (getAllItems config)
    , repoDeleteItemById = deleteItem config
    , repoUpdateItem = modifyItem config
    }

filesystemChecklistRepository :: ChecklistServiceConfig -> ChecklistRepository
filesystemChecklistRepository config =
  NotesChecklistRepository
    { repoCreateItem = createItem config
    , repoListItems = listItemsIgnoringParsingFailures (getAllItems config)
    , repoDeleteItemById = deleteItem config
    , repoUpdateItem = modifyItem config
    }

postgresNoteRepository :: Pool Connection -> NoteRepository
postgresNoteRepository pool = postgresNotesChecklistRepository pool "note_items"

postgresChecklistRepository :: Pool Connection -> ChecklistRepository
postgresChecklistRepository pool = postgresNotesChecklistRepository pool "checklist_items"

verifyPostgresNoteStorage :: Pool Connection -> IO (Either String ())
verifyPostgresNoteStorage pool = verifyPostgresStorageTable pool "note_items"

verifyPostgresChecklistStorage :: Pool Connection -> IO (Either String ())
verifyPostgresChecklistStorage pool = verifyPostgresStorageTable pool "checklist_items"

listItemsIgnoringParsingFailures
  :: ExceptT CrudReadException IO [ExceptT CrudReadException IO (Identifiable a)]
  -> ExceptT CrudReadException IO [Identifiable a]
listItemsIgnoringParsingFailures loadItems = do
  nested <- loadItems
  foldM accumulate [] nested
  where
    accumulate acc nestedRead = do
      readResult <- liftIO (runExceptT nestedRead)
      case readResult of
        Left err -> do
          liftIO (print ("Unexpected parsing exception: " ++ show err))
          pure acc
        Right item -> pure (item : acc)

postgresNotesChecklistRepository :: Content a => Pool Connection -> String -> NotesChecklistRepository a
postgresNotesChecklistRepository pool tableName =
  NotesChecklistRepository
    { repoCreateItem = pgCreateItem pool tableName
    , repoListItems = pgListItems pool tableName
    , repoDeleteItemById = pgDeleteItem pool tableName
    , repoUpdateItem = pgUpdateItem pool tableName
    }

verifyPostgresStorageTable :: Pool Connection -> String -> IO (Either String ())
verifyPostgresStorageTable pool tableName = do
  verifyResult <- Ex.try $ withResource pool $ \conn -> do
      pingResult <- timedTry ("SELECT ping-" ++ tableName) (query_ conn "SELECT 1" :: IO [Only Int]) :: IO (Either Ex.SomeException [Only Int])
      schemaResult <- timedTry ("SELECT schema-check-" ++ tableName)
        (query_ conn (buildSchemaCheckQuery tableName) :: IO [(String, String, String)])
        :: IO (Either Ex.SomeException [(String, String, String)])
      case pingResult of
        Left err -> pure (Left ("Postgres ping query failed: " ++ show err))
        Right _ ->
          case schemaResult of
            Left err -> pure (Left ("Schema check failed for " ++ tableName ++ ": " ++ show err))
            Right _ -> pure (Right ())
  case verifyResult of
    Left err -> pure (Left ("Unable to connect to Postgres: " ++ show (err :: Ex.SomeException)))
    Right value -> pure value

pgCreateItem :: Content a => Pool Connection -> String -> a -> ExceptT CrudWriteException IO StorageId
pgCreateItem pool tableName content =
  withPgConnection pool IOWriteException $ \conn -> do
    itemId <- liftIO (toString <$> nextRandom)
    let storeId = mkStorageId itemId content
    writeResult <- liftIO (timedTry ("INSERT " ++ tableName)
      (execute conn
        (buildInsertQuery tableName)
        ( itemId
        , version storeId
        , BL8.unpack (encode content)
        ))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure storeId

pgListItems :: Content a => Pool Connection -> String -> ExceptT CrudReadException IO [Identifiable a]
pgListItems pool tableName = do
  checkoutStartedAt <- liftIO getCurrentTime
  queryAndRows <- liftIO (Ex.try (withResource pool $ \conn -> do
      checkoutEndedAt <- getCurrentTime
      queryStartedAt <- getCurrentTime
      readResult <- timedTry ("SELECT " ++ tableName)
        (query_ conn (buildListQuery tableName) :: IO [(String, String, String)])
        :: IO (Either Ex.SomeException [(String, String, String)])
      queryEndedAt <- getCurrentTime
      pure (checkoutEndedAt, queryStartedAt, queryEndedAt, readResult)
    ) :: IO (Either Ex.SomeException (UTCTime, UTCTime, UTCTime, Either Ex.SomeException [(String, String, String)])))
  case queryAndRows of
    Left err -> throwError (IOReadException (userError (show err)))
    Right (checkoutEndedAt, queryStartedAt, queryEndedAt, readResult) -> do
      case readResult of
        Left err -> throwError (mapReadException err)
        Right rows -> do
          decodeStartedAt <- liftIO getCurrentTime
          decoded <- reverse <$> foldM accumulate [] rows
          decodeEndedAt <- liftIO getCurrentTime
          pure decoded
  where
    accumulate acc (itemId, itemVersion, rawContent) =
      case eitherDecode (BL8.pack rawContent) of
        Left err -> do
          liftIO (print ("Unexpected parsing exception: " ++ err))
          pure acc
        Right parsedContent ->
          pure (Identifiable StorageId {id = itemId, version = itemVersion} parsedContent : acc)

pgDeleteItem :: Pool Connection -> String -> String -> ExceptT CrudWriteException IO ()
pgDeleteItem pool tableName itemId =
  withPgConnection pool IOWriteException $ \conn -> do
    writeResult <- liftIO (timedTry ("DELETE " ++ tableName)
      (execute conn (buildDeleteQuery tableName) (Only itemId))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgUpdateItem :: Content a => Pool Connection -> String -> Identifiable a -> ExceptT CrudModificationException IO StorageId
pgUpdateItem pool tableName (Identifiable targetStorageId@StorageId {id = targetId, version = targetVersion} newContent) =
  withPgConnection pool mapConnectionError $ \conn -> do
    let newVersion = hash newContent
    writeResult <- liftIO (timedTry ("UPDATE " ++ tableName)
      (execute conn
        (buildUpdateQuery tableName)
        ( newVersion
        , BL8.unpack (encode newContent)
        , targetId
        , targetVersion
        ))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteToModificationException err)
      Right affected
        | affected > 0 -> pure targetStorageId {version = newVersion}
        | otherwise -> do
            latestResult <- liftIO (timedTry ("SELECT latest-version-" ++ tableName)
              (query conn (buildLookupVersionQuery tableName) (Only targetId) :: IO [Only String])
              :: IO (Either Ex.SomeException [Only String]))
            case latestResult of
              Left err -> throwError (CrudModificationReadingException (mapReadException err))
              Right [] -> throwError (CrudModificationReadingException (IOReadException (userError "Missing item id")))
              Right _ -> throwError (NotCurrentVersion targetStorageId)

buildSchemaCheckQuery :: String -> Query
buildSchemaCheckQuery tableName =
  fromString
    ("SELECT item_id, item_version, item_content::text FROM " ++ tableName ++ " LIMIT 0")

buildInsertQuery :: String -> Query
buildInsertQuery tableName =
  fromString
    ("INSERT INTO " ++ tableName ++ " (item_id, item_version, item_content) VALUES (?, ?, ?::jsonb)")

buildListQuery :: String -> Query
buildListQuery tableName =
  fromString
    ("SELECT item_id, item_version, item_content::text FROM " ++ tableName ++ " ORDER BY item_id")

buildDeleteQuery :: String -> Query
buildDeleteQuery tableName =
  fromString
    ("DELETE FROM " ++ tableName ++ " WHERE item_id = ?")

buildUpdateQuery :: String -> Query
buildUpdateQuery tableName =
  fromString
    ("UPDATE " ++ tableName ++ " SET item_version = ?, item_content = ?::jsonb WHERE item_id = ? AND item_version = ?")

buildLookupVersionQuery :: String -> Query
buildLookupVersionQuery tableName =
  fromString
    ("SELECT item_version FROM " ++ tableName ++ " WHERE item_id = ?")

mkStorageId :: Content a => String -> a -> StorageId
mkStorageId itemId contentToStore =
  StorageId
    { id = itemId
    , version = hash contentToStore
    }

withPgConnection
  :: forall e a. Pool Connection
  -> (IOError -> e)
  -> (Connection -> ExceptT e IO a)
  -> ExceptT e IO a
withPgConnection pool mapConnectionErr action = do
  runResult <- liftIO (Ex.try (withResource pool (\conn -> runExceptT (action conn))) :: IO (Either Ex.SomeException (Either e a)))
  case runResult of
    Left err -> throwError (mapConnectionErr (userError (show err)))
    Right (Left e) -> throwError e
    Right (Right value) -> pure value

mapReadException :: Ex.SomeException -> CrudReadException
mapReadException ex = IOReadException (userError (renderSqlError ex))

mapWriteException :: Ex.SomeException -> CrudWriteException
mapWriteException ex = IOWriteException (userError (renderSqlError ex))

mapConnectionError :: IOError -> CrudModificationException
mapConnectionError ioErr = CrudModificationReadingException (IOReadException ioErr)

mapWriteToModificationException :: Ex.SomeException -> CrudModificationException
mapWriteToModificationException ex =
  CrudModificationWritingException (mapWriteException ex)

renderSqlError :: Ex.SomeException -> String
renderSqlError ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr
      | isStorageSqlError sqlErr -> "Postgres storage failure: " ++ show sqlErr
      | otherwise -> show sqlErr
    Nothing -> show ex

isStorageSqlError :: SqlError -> Bool
isStorageSqlError sqlErr = "08" `BS8.isPrefixOf` sqlState sqlErr
