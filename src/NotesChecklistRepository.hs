{-# LANGUAGE ScopedTypeVariables #-}

module NotesChecklistRepository
  ( NotesChecklistRepository(..)
  , NoteRepository
  , ChecklistRepository
  , defaultNoteRepository
  , defaultChecklistRepository
  , filesystemNoteRepository
  , filesystemChecklistRepository
  ) where

import ChecklistCrud (ChecklistServiceConfig, defaultChecklistServiceConfig)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Crud (CrudModificationException, CrudReadException, CrudWriteException)
import CrudStorage (createItem, deleteItem, getAllItems, modifyItem)
import Model (ChecklistContent, Identifiable, NoteContent, StorageId)
import NoteCrud (NoteServiceConfig, defaultNoteServiceConfig)

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
