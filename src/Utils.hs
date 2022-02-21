module Utils where

import Control.Monad.IO.Class (liftIO)
import Futhark.Compiler (Imports)
import Language.LSP.Server (LspT)
import System.Log.Logger (debugM)

debug msg = liftIO $ debugM "futhark" msg

newtype State = State
  { stateProgram :: Maybe Imports
  }

emptyState :: State
emptyState = State Nothing
