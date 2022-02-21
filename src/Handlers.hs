{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Futhark.Compiler (Imports, Warnings, readProgram)
import Futhark.Compiler.CLI (runFutharkM)
import Futhark.Compiler.Config (Verbosity (NotVerbose))
import Futhark.FreshNames (VNameSource)
import Futhark.Pipeline (CompilerError (ExternalError), FutharkM)
import Futhark.Util.Loc (Pos (Pos), srclocOf)
import Language.Futhark.Query
import Language.Futhark.Syntax (locStr, pretty)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (Handlers, LspM, LspT, flushDiagnosticsBySource, notificationHandler, publishDiagnostics, requestHandler)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Utils

onInitializeHandler :: Handlers (LspM ())
onInitializeHandler = notificationHandler SInitialized $ \_msg -> debug "Initialized"

onHoverHandler :: MVar State -> Handlers (LspM ())
onHoverHandler state = requestHandler STextDocumentHover $ \req responder -> do
  debug "Got hover request"
  let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      Position l c = pos
      range = Range pos pos
      filePath = uriToFilePath $ doc ^. uri
  imports <- tryTakeImportsFromState state filePath
  result <- liftIO $ parseHoverInfoFromImports imports filePath (fromEnum l + 1) (fromEnum c)
  case result of
    Just msg -> do
      let ms = HoverContents $ MarkupContent MkMarkdown msg
          rsp = Hover ms (Just range)
      responder (Right $ Just rsp)
    Nothing -> responder (Right Nothing)

parseHoverInfoFromImports :: Maybe Imports -> Maybe FilePath -> Int -> Int -> IO (Maybe T.Text)
parseHoverInfoFromImports (Just imports) (Just path) l c = do
  case atPos imports $ Pos path l c 0 of
    Nothing -> pure $ Just "No information available"
    Just (AtName qn def _loc) -> do
      case def of
        Nothing -> pure $ Just ""
        Just (BoundTerm t defloc) -> do
          pure $ Just $ T.pack $ pretty qn ++ " :: " ++ pretty t ++ "\n\n" ++ "**Definition: " ++ locStr (srclocOf defloc) ++ "**"
        Just (BoundType defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModule defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModuleType defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
parseHoverInfoFromImports _ _ _ _ = pure $ Just "No information available"

onDocumentSaveHandler :: MVar State -> Handlers (LspM ())
onDocumentSaveHandler state = notificationHandler STextDocumentDidSave $ \msg -> do
  let NotificationMessage _ _ (DidSaveTextDocumentParams doc _text) = msg
      filePath = uriToFilePath $ doc ^. uri
  debug $ "Saved document" ++ show filePath
  debug "re-compiling"
  result <- tryCompile filePath
  case result of
    Nothing -> debug "Failed to re-compile, using previous state"
    Just imports -> do
      debug "Re-compile successful"
      liftIO $ swapMVar state (State (Just imports))
      pure ()

-- no re-compile
tryTakeImportsFromState :: MVar State -> Maybe FilePath -> LspT () IO (Maybe Imports)
tryTakeImportsFromState state filePath = do
  s <- liftIO $ takeMVar state
  case stateProgram s of
    Nothing -> do
      result <- tryCompile filePath
      liftIO $ putMVar state (s {stateProgram = result})
      pure result
    Just imports -> do
      liftIO $ putMVar state s
      pure $ Just imports

tryCompile :: Maybe FilePath -> LspT () IO (Maybe Imports)
tryCompile Nothing = pure Nothing
tryCompile (Just path) = do
  res <- liftIO $ runFutharkM (readProgram mempty path) NotVerbose
  case res of
    Right (_, imports, _) -> pure (Just imports)
    Left (ExternalError e) -> do
      debug "Compilation failed, publishing diagnostics"
      -- how to recover range from error?
      let diag = mkDiagnostic (Range (Position 0 0) (Position 0 10)) DsError (T.pack $ pretty e)
      sendDiagnostics (toNormalizedUri $ filePathToUri path) [diag]
      pure Nothing
    Left e -> do
      debug $ "Futhark compilation InternalError\n" ++ show e ++ "\nPlease contact support"
      pure Nothing

-- not sure what version do yet, put (Just 0) for now
sendDiagnostics :: NormalizedUri -> [Diagnostic] -> LspT () IO ()
sendDiagnostics uri diags = publishDiagnostics 100 uri (Just 0) (partitionBySource diags)

mkDiagnostic :: Range -> DiagnosticSeverity -> T.Text -> Diagnostic
mkDiagnostic rang severity msg = Diagnostic rang (Just severity) Nothing (Just "futhark") msg Nothing Nothing
