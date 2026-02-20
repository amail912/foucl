{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AgendaStorage
  ( CalendarStorageConfig(..)
  , defaultCalendarStorageConfig
  , CalendarStorageError(..)
  , createCalendarItem
  , getCalendarItems
  , updateCalendarItemDuration
  ) where

import AgendaModel (CalendarItem(..), CalendarItemContent(..))
import Control.Exception (IOException, try)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
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

createCalendarItem :: CalendarStorageConfig -> CalendarItemContent -> IO CalendarItem
createCalendarItem CalendarStorageConfig {calendarRootPath} content = do
  createDirectoryIfMissing True calendarRootPath
  newId <- generateNewId calendarRootPath
  let item = ServerCalendarItem { content = content, itemId = newId }
  BL.writeFile (calendarFilePath calendarRootPath newId) (encode item)
  pure item

getCalendarItems :: CalendarStorageConfig -> IO [CalendarItem]
getCalendarItems CalendarStorageConfig {calendarRootPath} = do
  createDirectoryIfMissing True calendarRootPath
  files <- listDirectory calendarRootPath
  let jsonFiles = filter (".json" `isSuffixOf`) files
  items <- mapM (readItemFromFile calendarRootPath) jsonFiles
  pure (catMaybes items)

updateCalendarItemDuration :: CalendarStorageConfig -> String -> Int -> IO (Either CalendarStorageError CalendarItem)
updateCalendarItemDuration CalendarStorageConfig {calendarRootPath} itemId minutes = do
  let path = calendarFilePath calendarRootPath itemId
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
              let updated = ServerCalendarItem { content = content { actualDurationMinutes = Just minutes }, itemId = itemId }
              writeResult <- try (BL.writeFile path (encode updated)) :: IO (Either IOException ())
              case writeResult of
                Left _ -> pure (Left CalendarItemWriteFailure)
                Right _ -> pure (Right updated)
            Just (NewCalendarItem {content}) -> do
              let updated = ServerCalendarItem { content = content { actualDurationMinutes = Just minutes }, itemId = itemId }
              writeResult <- try (BL.writeFile path (encode updated)) :: IO (Either IOException ())
              case writeResult of
                Left _ -> pure (Left CalendarItemWriteFailure)
                Right _ -> pure (Right updated)

calendarFilePath :: FilePath -> String -> FilePath
calendarFilePath root itemId = root </> itemId ++ ".json"

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
