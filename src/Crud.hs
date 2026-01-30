{-# LANGUAGE FunctionalDependencies #-}

module Crud ( DiskFileStorageConfig(..), CRUDEngine(..)
            , Error(..), CrudReadException(..), FromCrudReadException(..)
            , CrudModificationException(..), CrudWriteException(..), FromCrudWriteException(..)) where

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Maybe  (MaybeT)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Model                      (Content (..), Identifiable (..),
                                             StorageId (..))

class DiskFileStorageConfig a where
    rootPath :: a -> String

class (DiskFileStorageConfig crudType, Content a) => CRUDEngine crudType a | crudType -> a where
  getItems :: crudType -> ExceptT CrudReadException IO [ExceptT CrudReadException IO (Identifiable a)]
  postItem :: crudType -> a -> ExceptT CrudWriteException IO StorageId
  delItem :: crudType -> String -> ExceptT CrudWriteException IO ()
  putItem :: crudType -> Identifiable a -> ExceptT CrudModificationException IO StorageId
  crudTypeDenomination :: crudType -> String

newtype Error = FatalError String deriving (Eq, Show)

type ErrorMessage = String
type ParsedString = ByteString

data CrudReadException = IOReadException !IOError
                       | CrudParsingException !ParsedString !ErrorMessage !FilePath
                       deriving (Show, Eq)

newtype CrudWriteException = IOWriteException IOError deriving (Show, Eq)

data CrudModificationException = CrudModificationReadingException !CrudReadException
                               | CrudModificationWritingException !CrudWriteException
                               | NotCurrentVersion !StorageId
                               deriving (Show, Eq)

class FromCrudReadException a where
   fromCrudReadException :: CrudReadException -> a

instance FromCrudReadException CrudModificationException where
    fromCrudReadException = CrudModificationReadingException

class FromCrudWriteException a where
   fromCrudWriteException :: CrudWriteException -> a

instance FromCrudWriteException CrudModificationException where
    fromCrudWriteException = CrudModificationWritingException

