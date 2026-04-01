{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AgendaStorage
  ( CalendarStorageConfig(..)
  , defaultCalendarStorageConfig
  , CalendarStorageError(..)
  , createCalendarItem
  , deleteCalendarItem
  , getCalendarItems
  , updateCalendarItem
  , updateCalendarItemDuration
  ) where

import AgendaModel (CalendarItem(..), CalendarItemContent, applyActualDurationMinutes)
import Control.Exception (IOException, try)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>))

newtype CalendarStorageConfig = CalendarStorageConfig
  { calendarRootPath :: FilePath
  }

defaultCalendarStorageConfig :: CalendarStorageConfig
defaultCalendarStorageConfig = CalendarStorageConfig "data/calendar-items"

data CalendarStorageError
  = CalendarItemNotFound
  | CalendarItemReadFailure
  | CalendarItemWriteFailure
  deriving (Show, Eq)

createCalendarItem :: CalendarStorageConfig -> String -> CalendarItemContent -> IO CalendarItem
createCalendarItem config userId content = do
  let userRootPath = userCalendarRootPath config userId
  createDirectoryIfMissing True userRootPath
  newId <- generateNewId userRootPath
  let item = ServerCalendarItem { content = content, itemId = newId }
  BL.writeFile (calendarFilePath userRootPath newId) (encode item)
  pure item

getCalendarItems :: CalendarStorageConfig -> String -> IO [CalendarItem]
getCalendarItems config userId = do
  let userRootPath = userCalendarRootPath config userId
  createDirectoryIfMissing True userRootPath
  files <- listDirectory userRootPath
  let jsonFiles = filter (".json" `isSuffixOf`) files
  items <- mapM (readItemFromFile userRootPath) jsonFiles
  pure (catMaybes items)

updateCalendarItemDuration :: CalendarStorageConfig -> String -> String -> Int -> IO (Either CalendarStorageError CalendarItem)
updateCalendarItemDuration config userId itemId minutes = do
  let path = calendarFilePath (userCalendarRootPath config userId) itemId
  exists <- doesFileExist path
  if not exists
    then pure (Left CalendarItemNotFound)
    else do
      contentOrErr <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
      case contentOrErr of
        Left _ -> pure (Left CalendarItemReadFailure)
        Right raw ->
          case decode raw of
            Nothing -> pure (Left CalendarItemReadFailure)
            Just (ServerCalendarItem {content}) -> do
              let updated = ServerCalendarItem { content = applyActualDurationMinutes minutes content, itemId = itemId }
              writeResult <- try (BL.writeFile path (encode updated)) :: IO (Either IOException ())
              case writeResult of
                Left _ -> pure (Left CalendarItemWriteFailure)
                Right _ -> pure (Right updated)
            Just (NewCalendarItem {content}) -> do
              let updated = ServerCalendarItem { content = applyActualDurationMinutes minutes content, itemId = itemId }
              writeResult <- try (BL.writeFile path (encode updated)) :: IO (Either IOException ())
              case writeResult of
                Left _ -> pure (Left CalendarItemWriteFailure)
                Right _ -> pure (Right updated)

updateCalendarItem :: CalendarStorageConfig -> String -> String -> CalendarItemContent -> IO (Either CalendarStorageError CalendarItem)
updateCalendarItem config userId itemId content = do
  let path = calendarFilePath (userCalendarRootPath config userId) itemId
  exists <- doesFileExist path
  if not exists
    then pure (Left CalendarItemNotFound)
    else do
      writeResult <- try (BL.writeFile path (encode updated)) :: IO (Either IOException ())
      case writeResult of
                Left _ -> pure (Left CalendarItemWriteFailure)
                Right _ -> pure (Right updated)
  where
    updated = ServerCalendarItem { content = content, itemId = itemId }

deleteCalendarItem :: CalendarStorageConfig -> String -> String -> IO (Either CalendarStorageError ())
deleteCalendarItem config userId itemId = do
  let path = calendarFilePath (userCalendarRootPath config userId) itemId
  exists <- doesFileExist path
  if not exists
    then pure (Left CalendarItemNotFound)
    else do
      deleteResult <- try (removeFile path) :: IO (Either IOException ())
      case deleteResult of
        Left _ -> pure (Left CalendarItemWriteFailure)
        Right _ -> pure (Right ())

calendarFilePath :: FilePath -> String -> FilePath
calendarFilePath root itemId = root </> itemId ++ ".json"

userCalendarRootPath :: CalendarStorageConfig -> String -> FilePath
userCalendarRootPath CalendarStorageConfig {calendarRootPath} userId = calendarRootPath </> userId

generateNewId :: FilePath -> IO String
generateNewId root = do
  newId <- toString <$> nextRandom
  let path = calendarFilePath root newId
  exists <- doesFileExist path
  if exists
    then generateNewId root
    else pure newId

readItemFromFile :: FilePath -> FilePath -> IO (Maybe CalendarItem)
readItemFromFile root fileName = do
  let path = root </> fileName
  contentOrErr <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
  case contentOrErr of
    Left _ -> pure Nothing
    Right raw -> pure (decode raw)
