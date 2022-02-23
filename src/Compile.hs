module Compile (tryTakeStateFromMVar, tryReCompile) where

import Control.Concurrent.MVar (MVar, putMVar, swapMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Diagnostic (errorToDiagnostic, sendDiagnostics, warningsToDiagnostics)
import Futhark.Compiler (readProgram)
import Futhark.Pipeline (CompilerError (ExternalError), Verbosity (NotVerbose), runFutharkM)
import Futhark.Util.Pretty (pretty)
import Language.Futhark.Warnings (listWarnings)
import Language.LSP.Server (LspT)
import Language.LSP.Types (DiagnosticSeverity (DsError), filePathToUri, toNormalizedUri)
import Utils (State (..), debug, emptyState)

-- try to take state from MVar, if it's empty (Nothing), try to compile.
tryTakeStateFromMVar :: MVar State -> Maybe FilePath -> LspT () IO State
tryTakeStateFromMVar stateMVar filePath = do
  oldState <- liftIO $ takeMVar stateMVar
  case stateProgram oldState of
    Nothing -> do
      newState <- tryCompile filePath
      liftIO $ putMVar stateMVar newState
      pure newState
    Just imports -> do
      liftIO $ putMVar stateMVar oldState
      pure oldState

-- try to (re)-compile, replace old state if successful.
tryReCompile :: MVar State -> Maybe FilePath -> LspT () IO ()
tryReCompile stateMVar filePath = do
  debug "(Re)-compiling ..."
  newState <- tryCompile filePath
  case stateProgram newState of
    Nothing -> debug "Failed to (re)-compile, using previous state or Nothing"
    Just _ -> do
      debug "(Re)-compile successful"
      liftIO $ swapMVar stateMVar newState
      pure ()

-- try to compile file, publish diagnostics on warnings or error, return newly compiled state.
-- shouldn't be used in handlers directly.
tryCompile :: Maybe FilePath -> LspT () IO State
tryCompile Nothing = pure emptyState
tryCompile (Just path) = do
  res <- liftIO $ runFutharkM (readProgram mempty path) NotVerbose
  case res of
    Right (warnings, imports, _) -> do
      let diags = warningsToDiagnostics $ listWarnings warnings
      sendDiagnostics (toNormalizedUri $ filePathToUri path) diags
      pure $ State (Just imports)
    Left (ExternalError e) -> do
      debug "Compilation failed, publishing diagnostics"
      -- how to recover range from error?
      let diag = errorToDiagnostic
      sendDiagnostics (toNormalizedUri $ filePathToUri path) [diag]
      pure emptyState
    Left e -> do
      debug $ "Futhark compilation InternalError\n" ++ show e ++ "\nPlease contact support"
      pure emptyState
