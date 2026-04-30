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
  , notePostgresHealthChecks
  , checklistPostgresHealthChecks
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
import Data.Pool (Pool)
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
import Helpers (tryExcept, withPoolExceptHandled, withResourceMHandled)
import Text.Printf (printf)
import qualified Control.Exception as Ex

data NotesChecklistRepository a = NotesChecklistRepository
  { repoCreateItem :: !(a -> ExceptT CrudWriteException IO StorageId)
  , repoListItems :: !(ExceptT CrudReadException IO [Identifiable a])
  , repoDeleteItemById :: !(String -> ExceptT CrudWriteException IO ())
  , repoUpdateItem :: !(Identifiable a -> ExceptT CrudModificationException IO StorageId)
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

checklistPostgresHealthChecks :: Connection -> ExceptT String IO ()
checklistPostgresHealthChecks conn = do
  tryExcept (query_ conn "SELECT item_id, item_version, item_content::text FROM checklist_items LIMIT 0" :: IO [(String, String, String)])
            (\err -> "Schema check failed for checklist_items: " ++ show err)
  pure ()

notePostgresHealthChecks :: Connection -> ExceptT String IO ()
notePostgresHealthChecks conn = do
  tryExcept (query_ conn "SELECT item_id, item_version, item_content::text FROM note_items LIMIT 0" :: IO [(String, String, String)])
            (\err -> "Schema check failed for note_items: " ++ show err)
  pure ()

pgCreateItem :: Content a => Pool Connection -> String -> a -> ExceptT CrudWriteException IO StorageId
pgCreateItem pool tableName content =
  withPoolExceptHandled (IOWriteException . userError . show) pool $ \conn -> do
    itemId <- liftIO (toString <$> nextRandom)
    let storeId = mkStorageId itemId content
    writeResult <- tryExcept
      (execute conn
        (buildInsertQuery tableName)
        ( itemId
        , version storeId
        , BL8.unpack (encode content)
        ))
        mapWriteException
    pure storeId

pgListItems :: Content a => Pool Connection -> String -> ExceptT CrudReadException IO [Identifiable a]
pgListItems pool tableName = do
  withPoolExceptHandled
    (IOReadException . userError . show)
    pool
    (\conn -> do
      rows <- tryExcept
        (query_ conn (buildListQuery tableName) :: IO [(String, String, String)])
        mapReadException
      reverse <$> foldM accumulate [] rows
    )
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
  withPoolExceptHandled (IOWriteException . userError . show) pool $ \conn -> do
    writeResult <-tryExcept
      (execute conn (buildDeleteQuery tableName) (Only itemId))
      mapWriteException
    pure ()

pgUpdateItem :: Content a => Pool Connection -> String -> Identifiable a -> ExceptT CrudModificationException IO StorageId
pgUpdateItem pool tableName (Identifiable targetStorageId@StorageId {id = targetId, version = targetVersion} newContent) =
  withPoolExceptHandled (mapConnectionError . userError . show) pool $ \conn -> do
    let newVersion = hash newContent
    affected <- tryExcept
      (execute conn
        (buildUpdateQuery tableName)
        ( newVersion
        , BL8.unpack (encode newContent)
        , targetId
        , targetVersion
        ))
        mapWriteToModificationException
    if affected > 0
      then pure targetStorageId {version = newVersion}
      else do
        latestResult <- tryExcept (query conn (buildLookupVersionQuery tableName) (Only targetId) :: IO [Only String])
                                  (CrudModificationReadingException . mapReadException)
        case latestResult of
          [] -> throwError (CrudModificationReadingException (IOReadException (userError "Missing item id")))
          _ -> throwError (NotCurrentVersion targetStorageId)

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

mapReadException :: Ex.SomeException -> CrudReadException
mapReadException ex = IOReadException (userError (renderSqlError ex))

mapWriteException :: Ex.SomeException -> CrudWriteException
mapWriteException = IOWriteException . userError . renderSqlError

mapConnectionError :: IOError -> CrudModificationException
mapConnectionError = CrudModificationReadingException . IOReadException

mapWriteToModificationException :: Ex.SomeException -> CrudModificationException
mapWriteToModificationException = CrudModificationWritingException . mapWriteException

renderSqlError :: Ex.SomeException -> String
renderSqlError ex =
  case Ex.fromException ex :: Maybe SqlError of
    Just sqlErr
      | isStorageSqlError sqlErr -> "Postgres storage failure: " ++ show sqlErr
      | otherwise -> show sqlErr
    Nothing -> show ex

isStorageSqlError :: SqlError -> Bool
isStorageSqlError sqlErr = "08" `BS8.isPrefixOf` sqlState sqlErr
