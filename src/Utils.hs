module Utils
  ( debug,
    State (..),
    emptyState,
  )
where

import Control.Monad.IO.Class (liftIO)
import Futhark.Compiler.Program (LoadedProg)
import System.Log.Logger (debugM)

debug msg = liftIO $ debugM "futhark" msg

newtype State = State
  { stateProgram :: Maybe LoadedProg
  }

emptyState :: State
emptyState = State Nothing
