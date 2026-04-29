module Helpers
  ( tryExcept
  , withResourceM
  , withResourceMHandled
  , withPoolExceptHandled
  ) where

import qualified Control.Exception as Ex
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.Pool (Pool, withResource)

tryExcept :: IO a -> (Ex.SomeException -> e) -> ExceptT e IO a
tryExcept action onErr = do
  result <- lift (Ex.try action)
  either (throwE . onErr) pure result

withResourceM :: Pool a -> (a -> ExceptT e IO b) -> ExceptT e IO b
withResourceM pool action = ExceptT $ withResource pool (runExceptT . action)

withResourceMHandled :: (Ex.SomeException -> e) -> Pool a -> (a -> ExceptT e IO b) -> ExceptT e IO b
withResourceMHandled toErr pool action = ExceptT $ do
  result <- Ex.try (withResource pool (runExceptT . action))
  pure $ case result of
    Left ex -> Left (toErr ex)
    Right inner -> inner

withPoolExceptHandled :: (Ex.SomeException -> e) -> Pool a -> (a -> ExceptT e IO b) -> ExceptT e IO b
withPoolExceptHandled = withResourceMHandled
