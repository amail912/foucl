{-# LANGUAGE MultiParamTypeClasses #-}

module NoteCrud (NoteServiceConfig(..), defaultNoteServiceConfig) where

import           Crud        (CRUDEngine (..), DiskFileStorageConfig (..))
import           Model       (NoteContent (..))
import CrudStorage (getAllItems, createItem, deleteItem, modifyItem)

newtype NoteServiceConfig = NoteServiceConfig String

instance DiskFileStorageConfig NoteServiceConfig where
    rootPath (NoteServiceConfig path) = path

instance CRUDEngine NoteServiceConfig NoteContent where
  getItems = getAllItems
  postItem = createItem
  delItem = deleteItem
  putItem = modifyItem
  crudTypeDenomination _ = "note"

defaultNoteServiceConfig = NoteServiceConfig "data/note"
