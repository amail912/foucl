module SqlTiming
  ( timedTry
  ) where

import qualified Control.Exception as Ex
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

timedTry :: String -> IO a -> IO (Either Ex.SomeException a)
timedTry label action = do
  startedAt <- getCurrentTime
  result <- Ex.try action
  endedAt <- getCurrentTime
  let elapsedMs :: Double
      elapsedMs = realToFrac (diffUTCTime endedAt startedAt) * 1000
      outcome = either (const "error") (const "ok") result
  putStrLn ("[sql-timing] label=" ++ label ++ " duration_ms=" ++ printf "%.3f" elapsedMs ++ " outcome=" ++ outcome)
  pure result
