module Utils
  ( debug,
    State (..),
    emptyState,
    emptySemanticTokens,
    semanticTokensLegend,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Futhark.Compiler.Program (LoadedProg)
import Language.LSP.Types
  ( List (List),
    SemanticTokens (..),
    SemanticTokensLegend (..),
    knownSemanticTokenTypes,
  )
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
semanticTokensLegend :: SemanticTokensLegend
semanticTokensLegend =
  SemanticTokensLegend
    { _tokenTypes = List knownSemanticTokenTypes,
      _tokenModifiers = List []
    }
