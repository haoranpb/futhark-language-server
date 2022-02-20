module Utils where

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Server (LspT)
import System.Log.Logger (debugM)

debug msg = liftIO $ debugM "futhark" msg
