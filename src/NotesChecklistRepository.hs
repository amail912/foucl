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
import Control.Monad (foldM)
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
import Data.Pool (Pool)
import Data.String (fromString)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , Query
  , SqlError(..)
  , close
  , connectPostgreSQL
  , execute
  , query
  , query_
  )
import CrudStorage (createItem, deleteItem, getAllItems, modifyItem)
import Model (ChecklistContent, Content, Identifiable(..), NoteContent, StorageId(..), hash)
import NoteCrud (NoteServiceConfig, defaultNoteServiceConfig)
import SqlTiming (timedTry)
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

postgresNoteRepository :: Pool Connection -> String -> NoteRepository
postgresNoteRepository pool connectionString = postgresNotesChecklistRepository pool connectionString "note_items"

postgresChecklistRepository :: Pool Connection -> String -> ChecklistRepository
postgresChecklistRepository pool connectionString = postgresNotesChecklistRepository pool connectionString "checklist_items"

verifyPostgresNoteStorage :: String -> IO (Either String ())
verifyPostgresNoteStorage connectionString = verifyPostgresStorageTable connectionString "note_items"

verifyPostgresChecklistStorage :: String -> IO (Either String ())
verifyPostgresChecklistStorage connectionString = verifyPostgresStorageTable connectionString "checklist_items"

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

postgresNotesChecklistRepository :: Content a => Pool Connection -> String -> String -> NotesChecklistRepository a
postgresNotesChecklistRepository _ connectionString tableName =
  NotesChecklistRepository
    { repoCreateItem = pgCreateItem connectionString tableName
    , repoListItems = pgListItems connectionString tableName
    , repoDeleteItemById = pgDeleteItem connectionString tableName
    , repoUpdateItem = pgUpdateItem connectionString tableName
    }

verifyPostgresStorageTable :: String -> String -> IO (Either String ())
verifyPostgresStorageTable connectionString tableName = do
  connResult <- Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection)
  case connResult of
    Left err -> pure (Left ("Unable to connect to Postgres: " ++ show err))
    Right conn -> do
      pingResult <- timedTry ("SELECT ping-" ++ tableName) (query_ conn "SELECT 1" :: IO [Only Int]) :: IO (Either Ex.SomeException [Only Int])
      schemaResult <- timedTry ("SELECT schema-check-" ++ tableName)
        (query_ conn (buildSchemaCheckQuery tableName) :: IO [(String, String, String)])
        :: IO (Either Ex.SomeException [(String, String, String)])
      _ <- Ex.try (close conn) :: IO (Either Ex.SomeException ())
      case pingResult of
        Left err -> pure (Left ("Postgres ping query failed: " ++ show err))
        Right _ ->
          case schemaResult of
            Left err -> pure (Left ("Schema check failed for " ++ tableName ++ ": " ++ show err))
            Right _ -> pure (Right ())

pgCreateItem :: Content a => String -> String -> a -> ExceptT CrudWriteException IO StorageId
pgCreateItem connectionString tableName content =
  withPgConnection connectionString IOWriteException $ \conn -> do
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

pgListItems :: Content a => String -> String -> ExceptT CrudReadException IO [Identifiable a]
pgListItems connectionString tableName =
  withPgConnection connectionString IOReadException $ \conn -> do
    readResult <- liftIO (timedTry ("SELECT " ++ tableName)
      (query_ conn (buildListQuery tableName) :: IO [(String, String, String)])
      :: IO (Either Ex.SomeException [(String, String, String)]))
    case readResult of
      Left err -> throwError (mapReadException err)
      Right rows -> reverse <$> foldM accumulate [] rows
  where
    accumulate acc (itemId, itemVersion, rawContent) =
      case eitherDecode (BL8.pack rawContent) of
        Left err -> do
          liftIO (print ("Unexpected parsing exception: " ++ err))
          pure acc
        Right parsedContent ->
          pure (Identifiable StorageId {id = itemId, version = itemVersion} parsedContent : acc)

pgDeleteItem :: String -> String -> String -> ExceptT CrudWriteException IO ()
pgDeleteItem connectionString tableName itemId =
  withPgConnection connectionString IOWriteException $ \conn -> do
    writeResult <- liftIO (timedTry ("DELETE " ++ tableName)
      (execute conn (buildDeleteQuery tableName) (Only itemId))
      :: IO (Either Ex.SomeException Int64))
    case writeResult of
      Left err -> throwError (mapWriteException err)
      Right _ -> pure ()

pgUpdateItem :: Content a => String -> String -> Identifiable a -> ExceptT CrudModificationException IO StorageId
pgUpdateItem connectionString tableName (Identifiable targetStorageId@StorageId {id = targetId, version = targetVersion} newContent) =
  withPgConnection connectionString mapConnectionError $ \conn -> do
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
  :: String
  -> (IOError -> e)
  -> (Connection -> ExceptT e IO a)
  -> ExceptT e IO a
withPgConnection connectionString mapConnectionErr action = do
  connResult <- liftIO (Ex.try (connectPostgreSQL (BS8.pack connectionString)) :: IO (Either Ex.SomeException Connection))
  case connResult of
    Left err -> throwError (mapConnectionErr (userError (show err)))
    Right conn -> do
      runResult <- liftIO (runExceptT (action conn))
      _ <- liftIO (Ex.try (close conn) :: IO (Either Ex.SomeException ()))
      either throwError pure runResult

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
