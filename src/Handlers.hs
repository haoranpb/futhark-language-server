{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Futhark.Compiler (Imports, Warnings, readProgram, readProgramOrDie)
import Futhark.Compiler.CLI (runFutharkM)
import Futhark.Compiler.Config (Verbosity (NotVerbose))
import Futhark.FreshNames (VNameSource)
import Futhark.Pipeline (FutharkM)
import Futhark.Util.Loc (Pos (Pos), srclocOf)
import Language.Futhark.Query
import Language.Futhark.Syntax (locStr, pretty)
import Language.LSP.Server (Handlers, LspM, notificationHandler, requestHandler)
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
  result <- liftIO $ getHoverInfo (uriToFilePath $ doc ^. uri) state (fromEnum l + 1) (fromEnum c)
  case result of
    Just msg -> do
      let ms = HoverContents $ MarkupContent MkMarkdown msg
          rsp = Hover ms (Just range)
      responder (Right $ Just rsp)
    Nothing -> responder (Right Nothing)

getHoverInfo :: Maybe FilePath -> MVar State -> Int -> Int -> IO (Maybe T.Text)
getHoverInfo Nothing _ _ _ = do
  debug "No path" -- throw error
  pure Nothing
getHoverInfo (Just path) state l c = do
  result <- tryTakeImportsFromState state path
  case result of
    Nothing -> do
      debug "No imports"
      pure Nothing
    Just imports -> do
      case atPos imports $ Pos path l c 0 of
        Nothing -> pure $ Just "No information available"
        Just (AtName qn def _loc) -> do
          debug $ "Found " ++ show qn
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

onDocumentSaveHandler :: MVar State -> Handlers (LspM ())
onDocumentSaveHandler state = notificationHandler STextDocumentDidSave $ \msg -> do
  debug "Saved document"
  let NotificationMessage _ _ (DidSaveTextDocumentParams doc _text) = msg
      filePath = uriToFilePath $ doc ^. uri
  case filePath of
    Nothing -> debug "No path"
    Just path -> do
      debug $ "Saved document " ++ show path
      debug "re-compiling"
      liftIO $ do
        result <- tryCompile path
        case result of
          Nothing -> debug "Failed to re-compile, using previous state"
          Just imports -> do
            debug "Re-compile successful"
            swapMVar state (State (Just imports))
            pure ()
      pure ()

tryTakeImportsFromState :: MVar State -> FilePath -> IO (Maybe Imports)
tryTakeImportsFromState state path = do
  modifyMVar state $ \s -> do
    case stateProgram s of
      Nothing -> do
        result <- tryCompile path
        pure (s {stateProgram = result}, result)
      Just imports -> pure (s, Just imports)

tryCompile :: FilePath -> IO (Maybe Imports)
tryCompile file = do
  res <- runFutharkM (readProgram mempty file) NotVerbose
  case res of
    Left err -> do
      debug $ "Compilation failed\n" ++ show err
      pure Nothing
    Right (_, imports, _) -> pure (Just imports)
