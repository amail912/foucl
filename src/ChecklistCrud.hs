{-# LANGUAGE MultiParamTypeClasses #-}

module ChecklistCrud (ChecklistServiceConfig(..), defaultChecklistServiceConfig) where

import           Crud        (CRUDEngine (..), DiskFileStorageConfig (..))
import           Model       (ChecklistContent (..))
import CrudStorage (getAllItems, createItem, deleteItem, modifyItem)

newtype ChecklistServiceConfig = ChecklistServiceConfig String

instance DiskFileStorageConfig ChecklistServiceConfig where
    rootPath (ChecklistServiceConfig path) = path

instance CRUDEngine ChecklistServiceConfig ChecklistContent where
  getItems = getAllItems
  postItem = createItem
  delItem = deleteItem
  putItem = modifyItem
  crudTypeDenomination _ = "checklist"

defaultChecklistServiceConfig = ChecklistServiceConfig "data/checklist"
