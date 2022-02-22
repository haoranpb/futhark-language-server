{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Futhark.Compiler (Imports, readProgram)
import Futhark.Compiler.CLI (runFutharkM)
import Futhark.Compiler.Config (Verbosity (NotVerbose))
import Futhark.FreshNames (VNameSource)
import Futhark.Pipeline (CompilerError (ExternalError), FutharkM)
import Futhark.Util.Loc (Loc (Loc), Pos (Pos), SrcLoc, locOf, srclocOf)
import Futhark.Util.Pretty (Doc)
import Language.Futhark.Query
import Language.Futhark.Syntax (locStr, pretty)
import Language.Futhark.Warnings (Warnings, listWarnings)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server (Handlers, LspM, LspT, flushDiagnosticsBySource, notificationHandler, publishDiagnostics, requestHandler)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Utils

onInitializeHandler :: Handlers (LspM ())
onInitializeHandler = notificationHandler SInitialized $ \_msg -> debug "Initialized"

onHoverHandler :: MVar State -> Handlers (LspM ())
onHoverHandler stateMVar = requestHandler STextDocumentHover $ \req responder -> do
  debug "Got hover request"
  let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      Position l c = pos
      range = Range pos pos
      filePath = uriToFilePath $ doc ^. uri
  state <- tryTakeStateFromMVar stateMVar filePath
  result <- liftIO $ getHoverInfoFromImports (stateProgram state) filePath (fromEnum l + 1) (fromEnum c + 1)
  case result of
    Just msg -> do
      let ms = HoverContents $ MarkupContent MkMarkdown msg
          rsp = Hover ms (Just range)
      responder (Right $ Just rsp)
    Nothing -> responder (Right Nothing)

getHoverInfoFromImports :: Maybe Imports -> Maybe FilePath -> Int -> Int -> IO (Maybe T.Text)
getHoverInfoFromImports (Just imports) (Just path) l c = do
  case atPos imports $ Pos path l c 0 of
    Nothing -> pure $ Just "No information available"
    Just (AtName qn def _loc) -> do
      case def of
        Nothing -> pure $ Just "No information available"
        Just (BoundTerm t defloc) -> do
          pure $ Just $ T.pack $ pretty qn ++ " :: " ++ pretty t ++ "\n\n" ++ "**Definition: " ++ locStr (srclocOf defloc) ++ "**"
        Just (BoundType defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModule defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModuleType defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
getHoverInfoFromImports _ _ _ _ = pure $ Just "No information available"

onDocumentSaveHandler :: MVar State -> Handlers (LspM ())
onDocumentSaveHandler stateMVar = notificationHandler STextDocumentDidSave $ \msg -> do
  let NotificationMessage _ _ (DidSaveTextDocumentParams doc _text) = msg
      filePath = uriToFilePath $ doc ^. uri
  debug $ "Saved document" ++ show filePath
  debug "re-compiling"
  newState <- tryCompile filePath
  case stateProgram newState of
    Nothing -> debug "Failed to re-compile, using previous state"
    Just _ -> do
      debug "Re-compile successful"
      liftIO $ swapMVar stateMVar newState
      pure ()

onCompletionHandler :: MVar State -> Handlers (LspM ())
onCompletionHandler stateMVar = requestHandler STextDocumentCompletion $ \req responder -> do
  debug "Got completion request"
  let RequestMessage _ _ _ (CompletionParams doc pos _workDone _ _) = req
      completionItem = mkCompletionItem "hello futhark"
  responder $ Right $ InL $ List [completionItem]

-- no re-compile
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

tryCompile :: Maybe FilePath -> LspT () IO State
tryCompile Nothing = pure emptyState
tryCompile (Just path) = do
  res <- liftIO $ runFutharkM (readProgram mempty path) NotVerbose
  case res of
    Right (warnings, imports, _) -> do
      debug $ pretty warnings
      let diags = warningsToDiagnostics $ listWarnings warnings
      sendDiagnostics (toNormalizedUri $ filePathToUri path) diags
      pure $ State (Just imports)
    Left (ExternalError e) -> do
      debug "Compilation failed, publishing diagnostics"
      -- how to recover range from error?
      let diag = mkDiagnostic (Range (Position 0 0) (Position 0 10)) DsError (T.pack $ pretty e)
      sendDiagnostics (toNormalizedUri $ filePathToUri path) [diag]
      pure emptyState
    Left e -> do
      debug $ "Futhark compilation InternalError\n" ++ show e ++ "\nPlease contact support"
      pure emptyState

-- not sure what version do yet, put (Just 0) for now
sendDiagnostics :: NormalizedUri -> [Diagnostic] -> LspT () IO ()
sendDiagnostics uri diags = publishDiagnostics 100 uri (Just 0) (partitionBySource diags)

mkDiagnostic :: Range -> DiagnosticSeverity -> T.Text -> Diagnostic
mkDiagnostic range severity msg = Diagnostic range (Just severity) Nothing (Just "futhark") msg Nothing Nothing

mkCompletionItem :: T.Text -> CompletionItem
mkCompletionItem label = CompletionItem label (Just CiText) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

warningsToDiagnostics :: [(SrcLoc, [SrcLoc], Doc)] -> [Diagnostic]
warningsToDiagnostics =
  map
    ( \(srcloc, _, msg) -> do
        mkDiagnostic (rangeFromSrcLoc srcloc) DsWarning (T.pack $ pretty msg)
    )

-- the ending appears to be one col too short
rangeFromSrcLoc :: SrcLoc -> Range
rangeFromSrcLoc srcloc = do
  let Loc start end = locOf srcloc
  Range (getPosition start) (getPosition end)

getPosition :: Pos -> Position
getPosition pos = do
  let Pos _ line col _ = pos
  Position (toEnum line - 1) (toEnum col - 1)
