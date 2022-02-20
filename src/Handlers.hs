{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Futhark.Compiler (Imports, readProgramOrDie)
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
  msg <- liftIO $ getHoverInfo (uriToFilePath $ doc ^. uri) state (fromEnum l + 1) (fromEnum c)
  let ms = HoverContents $ MarkupContent MkMarkdown msg
      rsp = Hover ms (Just range)
  responder (Right $ Just rsp)

getHoverInfo :: Maybe FilePath -> MVar State -> Int -> Int -> IO T.Text
getHoverInfo Nothing _ _ _ = do
  debug "No path" -- throw error
  pure "404 FilePath not found"
getHoverInfo (Just path) state l c = do
  imports <- takeImportsFromState state path
  case atPos imports $ Pos path l c 0 of
    Nothing -> pure "No information available"
    Just (AtName qn def _loc) -> do
      debug $ "Found " ++ show qn
      case def of
        Nothing -> pure ""
        Just (BoundTerm t defloc) -> do
          pure $ T.pack $ pretty qn ++ " :: " ++ pretty t ++ "\n\n" ++ "**Definition: " ++ locStr (srclocOf defloc) ++ "**"
        Just (BoundType defloc) ->
          pure $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModule defloc) ->
          pure $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModuleType defloc) ->
          pure $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)

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
      (_, imports, _) <- readProgramOrDie path
      liftIO $ swapMVar state (State (Just imports))
      pure ()

onDocumentOpenHandler :: MVar State -> Handlers (LspM ())
onDocumentOpenHandler state = notificationHandler STextDocumentDidOpen $ \msg -> debug "Opened document"

onDocumentChangeHandler :: MVar State -> Handlers (LspM ())
onDocumentChangeHandler state = notificationHandler STextDocumentDidSave $ \msg -> debug "Changed document"

onDocumentCloseHandler :: MVar State -> Handlers (LspM ())
onDocumentCloseHandler state = notificationHandler STextDocumentDidClose $ \msg -> debug "Closed document"

takeImportsFromState :: MVar State -> FilePath -> IO Imports
takeImportsFromState state path = do
  s <- takeMVar state
  case stateProgram s of
    Nothing -> do
      debug "Empty state, compiling ..."
      (_, imports, _) <- readProgramOrDie path
      -- put Nothing when Error
      putMVar state (State (Just imports))
      pure imports
    Just imports -> do
      putMVar state (State (Just imports))
      pure imports
