{-# LANGUAGE OverloadedStrings #-}

module Lib.Startup.Import.NotesChecklist
  ( runNoteStartupImport
  , runChecklistStartupImport
  ) where

import Prelude hiding (id)
import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (decode, encode)
import Data.List (sort, sortOn)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Control.Exception as Ex
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection, Only(..), execute, query_)
import Helpers (tryExcept, withResourceMHandled)
import Model (ChecklistContent, Identifiable(..), NoteContent)
import qualified Model
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)

runNoteStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runNoteStartupImport pool cd = do
  let notesBaseDir = cd </> "data" </> "note"
  fsEntries <- loadFilesystemNoteImportEntries notesBaseDir
  (importedCount, skippedCount, totalPgRows) <-
    withResourceMHandled
      (\err -> "Note startup import failed while connecting to Postgres: " ++ show err)
      pool
      (\conn -> do
        pgRows <- tryExcept (query_ conn "SELECT item_id FROM note_items" :: IO [Only String])
                            (\err -> "Note startup import failed while reading Postgres notes: " ++ show err)
        let orderedFsEntries = sortOn (\(itemId, _, _) -> itemId) fsEntries
            knownIds = Set.fromList (map fromOnly pgRows)
        when (not (null orderedFsEntries) && not (null pgRows)) $
          lift $ putStrLn
            ( "[startup][note-import][warning] overlap detected:"
                ++ " filesystem_count="
                ++ show (length orderedFsEntries)
                ++ " postgres_count="
                ++ show (length pgRows)
                ++ " conflict_policy=postgres-wins"
            )
        (i, s, _) <- foldM (importSingleNoteItem conn) (0 :: Int, 0 :: Int, knownIds) orderedFsEntries
        pure (i, s, length pgRows)
      )
  lift $ putStrLn
    ( "[startup][note-import] completed"
        ++ " filesystem_count="
        ++ show (length fsEntries)
        ++ " postgres_count="
        ++ show totalPgRows
        ++ " imported="
        ++ show importedCount
        ++ " skipped_conflicts="
        ++ show skippedCount
    )

runChecklistStartupImport :: Pool Connection -> FilePath -> ExceptT String IO ()
runChecklistStartupImport pool cd = do
  let checklistsBaseDir = cd </> "data" </> "checklist"
  fsEntries <- loadFilesystemChecklistImportEntries checklistsBaseDir
  (importedCount, skippedCount, totalPgRows) <-
    withResourceMHandled
      (\err -> "Checklist startup import failed while connecting to Postgres: " ++ show err)
      pool
      (\conn -> do
        pgRows <- tryExcept (query_ conn "SELECT item_id FROM checklist_items" :: IO [Only String])
                            (\err -> "Checklist startup import failed while reading Postgres checklists: " ++ show err)
        let orderedFsEntries = sortOn (\(itemId, _, _) -> itemId) fsEntries
            knownIds = Set.fromList (map fromOnly pgRows)
        when (not (null orderedFsEntries) && not (null pgRows)) $
          lift $ putStrLn
            ( "[startup][checklist-import][warning] overlap detected:"
                ++ " filesystem_count="
                ++ show (length orderedFsEntries)
                ++ " postgres_count="
                ++ show (length pgRows)
                ++ " conflict_policy=postgres-wins"
            )
        (i, s, _) <- foldM (importSingleChecklistItem conn) (0 :: Int, 0 :: Int, knownIds) orderedFsEntries
        pure (i, s, length pgRows)
      )
  lift $ putStrLn
    ( "[startup][checklist-import] completed"
        ++ " filesystem_count="
        ++ show (length fsEntries)
        ++ " postgres_count="
        ++ show totalPgRows
        ++ " imported="
        ++ show importedCount
        ++ " skipped_conflicts="
        ++ show skippedCount
    )

importSingleNoteItem
  :: Connection
  -> (Int, Int, Set.Set String)
  -> (String, String, NoteContent)
  -> ExceptT String IO (Int, Int, Set.Set String)
importSingleNoteItem conn (importedCount, skippedCount, knownIds) (itemId, itemVersion, noteContent) =
  if Set.member itemId knownIds
    then do
      lift $ putStrLn ("[startup][note-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownIds)
    else do
      affected <- tryExcept (execute conn
                              "INSERT INTO note_items (item_id, item_version, item_content) VALUES (?, ?, ?::jsonb) ON CONFLICT (item_id) DO NOTHING"
                              (itemId, itemVersion, BL8.unpack (encode noteContent)))
                            (\err -> "Note startup import failed while writing item_id=" ++ itemId ++ ": " ++ show err)
      if affected > 0
        then
          pure (importedCount + 1, skippedCount, Set.insert itemId knownIds)
        else do
          lift $ putStrLn ("[startup][note-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert itemId knownIds)

importSingleChecklistItem
  :: Connection
  -> (Int, Int, Set.Set String)
  -> (String, String, ChecklistContent)
  -> ExceptT String IO (Int, Int, Set.Set String)
importSingleChecklistItem conn (importedCount, skippedCount, knownIds) (itemId, itemVersion, checklistContent) =
  if Set.member itemId knownIds
    then do
      lift $ putStrLn ("[startup][checklist-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
      pure (importedCount, skippedCount + 1, knownIds)
    else do
      affected <- tryExcept (execute conn
                              "INSERT INTO checklist_items (item_id, item_version, item_content) VALUES (?, ?, ?::jsonb) ON CONFLICT (item_id) DO NOTHING"
                              (itemId, itemVersion, BL8.unpack (encode checklistContent)))
                            (\err -> "Checklist startup import failed while writing item_id=" ++ itemId ++ ": " ++ show err)
      if affected > 0
        then
          pure (importedCount + 1, skippedCount, Set.insert itemId knownIds)
        else do
          lift $ putStrLn ("[startup][checklist-import][warning] skipping conflicting item_id=" ++ itemId ++ " policy=postgres-wins")
          pure (importedCount, skippedCount + 1, Set.insert itemId knownIds)

loadFilesystemNoteImportEntries :: FilePath -> ExceptT String IO [(String, String, NoteContent)]
loadFilesystemNoteImportEntries rootDir = do
  exists <- lift $ doesDirectoryExist rootDir
  if not exists
    then do
      lift $ putStrLn "[startup][note-import] source note directory is missing; treating filesystem note source as empty"
      pure []
    else do
      files <- lift $ listDirectory rootDir
      foldM (decodeSingleFilesystemNote rootDir) [] (sort files)

decodeSingleFilesystemNote
  :: FilePath
  -> [(String, String, NoteContent)]
  -> FilePath
  -> ExceptT String IO [(String, String, NoteContent)]
decodeSingleFilesystemNote rootDir acc fileName
  | takeExtension fileName /= ".txt" = pure acc
  | otherwise = do
      let fullPath = rootDir </> fileName
      contentResult <- lift (Ex.try (BL.readFile fullPath) :: IO (Either Ex.IOException BL.ByteString))
      case contentResult of
        Left err ->
          throwE ("Note startup import failed while reading filesystem note file " ++ fullPath ++ ": " ++ show err)
        Right raw ->
          case decode raw of
            Nothing ->
              throwE ("Note startup import failed while reading filesystem notes: invalid JSON in " ++ fullPath)
            Just noteItem ->
              let itemStorageId = storageId (noteItem :: Identifiable NoteContent)
                  itemId = Model.id itemStorageId
                  itemVersion = Model.version itemStorageId
               in pure ((itemId, itemVersion, content noteItem) : acc)

loadFilesystemChecklistImportEntries :: FilePath -> ExceptT String IO [(String, String, ChecklistContent)]
loadFilesystemChecklistImportEntries rootDir = do
  exists <- lift $ doesDirectoryExist rootDir
  if not exists
    then do
      lift $ putStrLn "[startup][checklist-import] source checklist directory is missing; treating filesystem checklist source as empty"
      pure []
    else do
      files <- lift $ listDirectory rootDir
      foldM (decodeSingleFilesystemChecklist rootDir) [] (sort files)

decodeSingleFilesystemChecklist
  :: FilePath
  -> [(String, String, ChecklistContent)]
  -> FilePath
  -> ExceptT String IO [(String, String, ChecklistContent)]
decodeSingleFilesystemChecklist rootDir acc fileName
  | takeExtension fileName /= ".txt" = pure acc
  | otherwise = do
      let fullPath = rootDir </> fileName
      contentResult <- lift (Ex.try (BL.readFile fullPath) :: IO (Either Ex.IOException BL.ByteString))
      case contentResult of
        Left err ->
          throwE ("Checklist startup import failed while reading filesystem checklist file " ++ fullPath ++ ": " ++ show err)
        Right raw ->
          case decode raw of
            Nothing ->
              throwE ("Checklist startup import failed while reading filesystem checklists: invalid JSON in " ++ fullPath)
            Just checklistItem ->
              let itemStorageId = storageId (checklistItem :: Identifiable ChecklistContent)
                  itemId = Model.id itemStorageId
                  itemVersion = Model.version itemStorageId
               in pure ((itemId, itemVersion, content checklistItem) : acc)
