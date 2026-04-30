module Lib.Startup.Import.Common
  ( withErrPrefix
  ) where

import Control.Monad.Trans.Except (ExceptT, catchE, throwE)

withErrPrefix :: String -> ExceptT String IO a -> ExceptT String IO a
withErrPrefix prefix action =
  catchE action (\err -> throwE (prefix ++ err))
