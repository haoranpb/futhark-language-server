module Utils
  ( debug,
    State (..),
    emptyState,
    emptySemanticTokens,
    SemanticTokenTypes (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Futhark.Compiler.Program (LoadedProg)
import Language.LSP.Types (List (List), SemanticTokens (..))
import System.Log.Logger (debugM)

debug :: Control.Monad.IO.Class.MonadIO m => String -> m ()
debug msg = liftIO $ debugM "futhark" msg

newtype State = State
  { stateProgram :: Maybe LoadedProg
  }

emptyState :: State
emptyState = State Nothing

emptySemanticTokens :: SemanticTokens
emptySemanticTokens = SemanticTokens Nothing (List [])

-- Define the SemanticTokenTypes, the one from Language.LSP.Types doesn't derive Enum
--https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokenTypes
data SemanticTokenTypes
  = Namespace
  | Type
  | Class
  | Enum
  | Interface
  | Struct
  | TypeParameter
  | Parameter
  | Variable
  | Property
  | EnumNumber
  | Event
  | Function
  | Method
  | Macro
  | Keyword
  | Modifier
  | Comment
  | String
  | Number
  | Regexp
  | Operator
  deriving (Enum)
