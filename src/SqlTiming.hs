module SqlTiming
  ( timedTry
  ) where

import qualified Control.Exception as Ex

timedTry :: String -> IO a -> IO (Either Ex.SomeException a)
timedTry _label = Ex.try
