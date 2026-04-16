{-# LANGUAGE NamedFieldPuns #-}

module CalendarRepository
  ( CalendarRepository(..)
  , defaultCalendarRepository
  , filesystemCalendarRepository
  ) where

import AgendaStorage
  ( CalendarStorageConfig(..)
  , CalendarStorageError(..)
  , createCalendarItem
  , defaultCalendarStorageConfig
  , deleteCalendarItem
  , getCalendarItems
  , updateCalendarItem
  , updateCalendarItemDuration
  )
import qualified AgendaModel as Agenda
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Repository (RepositoryError(..))
import qualified Control.Exception as Ex


data CalendarRepository = CalendarRepository
  { repoCreateCalendarItem :: String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoLoadCalendarItemById :: String -> String -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoListCalendarItemsForUser :: String -> ExceptT RepositoryError IO [Agenda.CalendarItem]
  , repoUpdateCalendarItem :: String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoUpdateCalendarItemDuration :: String -> String -> Int -> ExceptT RepositoryError IO Agenda.CalendarItem
  , repoDeleteCalendarItemById :: String -> String -> ExceptT RepositoryError IO ()
  }

defaultCalendarRepository :: CalendarRepository
defaultCalendarRepository = filesystemCalendarRepository defaultCalendarStorageConfig

filesystemCalendarRepository :: CalendarStorageConfig -> CalendarRepository
filesystemCalendarRepository config =
  CalendarRepository
    { repoCreateCalendarItem = fsCreateCalendarItem config
    , repoLoadCalendarItemById = fsLoadCalendarItemById config
    , repoListCalendarItemsForUser = fsListCalendarItemsForUser config
    , repoUpdateCalendarItem = fsUpdateCalendarItem config
    , repoUpdateCalendarItemDuration = fsUpdateCalendarItemDuration config
    , repoDeleteCalendarItemById = fsDeleteCalendarItemById config
    }

fsCreateCalendarItem :: CalendarStorageConfig -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
fsCreateCalendarItem config userId content = do
  result <- liftIO (Ex.try (createCalendarItem config userId content) :: IO (Either Ex.IOException Agenda.CalendarItem))
  case result of
    Left _ -> throwError WriteFailure
    Right item -> pure item

fsLoadCalendarItemById :: CalendarStorageConfig -> String -> String -> ExceptT RepositoryError IO Agenda.CalendarItem
fsLoadCalendarItemById config userId itemId = do
  items <- fsListCalendarItemsForUser config userId
  case filter matchesItemId items of
    [] -> throwError NotFound
    (item:_) -> pure item
  where
    matchesItemId item =
      case item of
        Agenda.ServerCalendarItem {Agenda.itemId = sid} -> sid == itemId
        Agenda.NewCalendarItem {} -> False

fsListCalendarItemsForUser :: CalendarStorageConfig -> String -> ExceptT RepositoryError IO [Agenda.CalendarItem]
fsListCalendarItemsForUser config userId = do
  result <- liftIO (Ex.try (getCalendarItems config userId) :: IO (Either Ex.IOException [Agenda.CalendarItem]))
  case result of
    Left _ -> throwError ReadFailure
    Right items -> pure (sortOn calendarItemSortKey items)

fsUpdateCalendarItem :: CalendarStorageConfig -> String -> String -> Agenda.CalendarItemContent -> ExceptT RepositoryError IO Agenda.CalendarItem
fsUpdateCalendarItem config userId itemId content = do
  result <- liftIO (updateCalendarItem config userId itemId content)
  case result of
    Left err -> throwError (mapCalendarError err)
    Right item -> pure item

fsUpdateCalendarItemDuration :: CalendarStorageConfig -> String -> String -> Int -> ExceptT RepositoryError IO Agenda.CalendarItem
fsUpdateCalendarItemDuration config userId itemId minutes = do
  result <- liftIO (updateCalendarItemDuration config userId itemId minutes)
  case result of
    Left err -> throwError (mapCalendarError err)
    Right item -> pure item

fsDeleteCalendarItemById :: CalendarStorageConfig -> String -> String -> ExceptT RepositoryError IO ()
fsDeleteCalendarItemById config userId itemId = do
  result <- liftIO (deleteCalendarItem config userId itemId)
  case result of
    Left err -> throwError (mapCalendarError err)
    Right () -> pure ()

mapCalendarError :: CalendarStorageError -> RepositoryError
mapCalendarError CalendarItemNotFound = NotFound
mapCalendarError CalendarItemReadFailure = ReadFailure
mapCalendarError CalendarItemWriteFailure = WriteFailure

calendarItemSortKey :: Agenda.CalendarItem -> String
calendarItemSortKey item =
  case item of
    Agenda.ServerCalendarItem {Agenda.itemId} -> itemId
    Agenda.NewCalendarItem {} -> ""
