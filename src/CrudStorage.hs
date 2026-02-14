{-# LANGUAGE ScopedTypeVariables #-}

module CrudStorage (createItem, getAllItems, deleteItem, modifyItem) where

import           Control.Exception          (catch)
import           Control.Monad              (mzero)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import           Control.Monad.Trans.Either (EitherT, hoistEither, firstEitherT, handleEitherT)
import           Crud                       (CRUDEngine (..),
                                             CrudModificationException (..),
                                             CrudReadException (..),
                                             CrudWriteException (..),
                                             DiskFileStorageConfig (..),
                                             Error (..),
                                             FromCrudReadException (..),
                                             FromCrudWriteException (..))
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             encode)
import           Data.ByteString.Lazy.Char8 as BL hiding (appendFile, filter,
                                                   map, putStrLn)
import           Data.Maybe                 (fromJust)
import qualified Data.UUID                  as UUID (toString)
import qualified Data.UUID.V4               as UUID (nextRandom)
import           Model                      (ChecklistContent (..), Content,
                                             Identifiable (..),
                                             NoteContent (..), StorageId (..),
                                             hash)
import qualified Prelude                    (id)
import           Prelude                    hiding (id, log, readFile,
                                             writeFile)
import           System.Directory           (createDirectoryIfMissing,
                                             listDirectory, removePathForcibly,
                                             doesFileExist)

-- DATA
type Id = String

-- PRIVATE
-- A fileName with its extension but without its containing directory path (e.g. "26dfc71b-63aa-479d-b1c0-3d90eec1083c.txt")
type SimpleFileName = String

-- FUNCTIONS
createItem :: CRUDEngine crudConfig a => crudConfig -> a -> EitherT CrudWriteException IO StorageId
createItem config nc = do
    handleEitherT IOWriteException
                  (createDirectoryIfMissing True (rootPath config))
    uuid <- firstEitherT IOWriteException $ generateNewUUID config
    writeContentToFile config nc uuid

getAllItems :: CRUDEngine crudConfig a => crudConfig -> EitherT CrudReadException IO [EitherT CrudReadException IO (Identifiable a)]
getAllItems config = do
    liftIO $ catch (createDirectoryIfMissing True (rootPath config))
                   (\(e :: IOError) -> return ())
    simpleFileNames <- firstEitherT IOReadException $ listFiles config
    return $ fmap (readItemFromFile config) simpleFileNames

deleteItem :: DiskFileStorageConfig confType => confType -> String -> EitherT CrudWriteException IO ()
deleteItem config id = handleEitherT IOWriteException (removePathForcibly $ fileName config id)

modifyItem :: CRUDEngine crudConfig a => crudConfig -> Identifiable a -> EitherT CrudModificationException IO StorageId
modifyItem config (Identifiable targetStorageId new) = do
    log ("modifying file with id " ++ show targetStorageId)
    (Identifiable retrievedStorageId content) <- firstEitherT fromCrudReadException $ readItemFromFile config (id targetStorageId ++ txtExtension)
    if retrievedStorageId == targetStorageId
        then firstEitherT fromCrudWriteException $ writeContentToFile config new (id targetStorageId)
        else throwError $ NotCurrentVersion targetStorageId


writeContentToFile :: CRUDEngine crudConfig a => crudConfig -> a -> String -> EitherT CrudWriteException IO StorageId
writeContentToFile storageConfig content fileId = do
    let storeId = StorageId { id = fileId, version = hash content }
    handleEitherT IOWriteException
                  (writeFile (fileName storageConfig fileId) (encode $ Identifiable storeId content))
    return storeId

readItemFromFile :: CRUDEngine crudConfig a => crudConfig -> SimpleFileName -> EitherT CrudReadException IO (Identifiable a)
readItemFromFile config file = do
    stringToParse <- (readFileImpl . prefixWithStorageDir config) file
    firstEitherT (parsingExceptionToCrudException file) $ parseString config stringToParse

parseString :: CRUDEngine crudConfig a => crudConfig -> ByteString -> EitherT ParsingException IO (Identifiable a)
parseString config strToParse = firstEitherT (ParsingException strToParse)
                                             (hoistEither $ eitherDecode strToParse)

data ParsingException = ParsingException !ParsedString !ErrorMessage
type ErrorMessage = String
type ParsedString = ByteString

parsingExceptionToCrudException :: FilePath -> ParsingException -> CrudReadException
parsingExceptionToCrudException file (ParsingException parsedString errorMessage) =
    CrudParsingException parsedString errorMessage file

listFiles :: DiskFileStorageConfig crudConfig => crudConfig -> EitherT IOError IO [String]
listFiles config = handleEitherT ioError
                                 (listDirectory (rootPath config))
  where ioError :: IOError -> IOError
        ioError = Prelude.id


readFileImpl :: FilePath -> EitherT CrudReadException IO ByteString
readFileImpl file = handleEitherT IOReadException
                                  (readFile file)

generateNewUUID :: DiskFileStorageConfig crudConfig => crudConfig -> EitherT IOError IO String
generateNewUUID config = do
  newUUID <- liftIO $ fmap UUID.toString UUID.nextRandom
  fileExists <- liftIO $ doesFileExist (fileName config newUUID)
  if fileExists
    then do
      log "uuid already exists. Generating a new one"
      generateNewUUID config
    else return newUUID

fileName :: DiskFileStorageConfig crudConfig => crudConfig -> Id -> FilePath
fileName storageConfig id = prefixWithStorageDir storageConfig id ++ txtExtension

txtExtension :: String
txtExtension = ".txt"

prefixWithStorageDir :: DiskFileStorageConfig crudConfig => crudConfig -> SimpleFileName -> String
prefixWithStorageDir storageConfig s = postFixWithIfNeeded '/' (rootPath storageConfig) ++ s
    where
        postFixWithIfNeeded c []     = [c]
        postFixWithIfNeeded c [x] = if c == x then [c] else [x, c]
        postFixWithIfNeeded c (x:xs) = x:postFixWithIfNeeded c xs

log :: MonadIO m => String -> m ()
log s = liftIO $ appendFile  "./server.log" $ s ++ "\n"

