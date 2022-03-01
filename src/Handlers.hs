{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Compile (tryReCompile, tryTakeStateFromMVar)
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Futhark.Util.Pretty (pretty)
import Language.LSP.Server (Handlers, LspM, notificationHandler, requestHandler)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Tool (getHoverInfoFromState)
import Utils (State (..), debug)

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
  result <- liftIO $ getHoverInfoFromState state filePath (fromEnum l + 1) (fromEnum c + 1)
  case result of
    Just msg -> do
      let ms = HoverContents $ MarkupContent MkMarkdown msg
          rsp = Hover ms (Just range)
      responder (Right $ Just rsp)
    Nothing -> responder (Right Nothing)

onDocumentSaveHandler :: MVar State -> Handlers (LspM ())
onDocumentSaveHandler stateMVar = notificationHandler STextDocumentDidSave $ \msg -> do
  let NotificationMessage _ _ (DidSaveTextDocumentParams doc _text) = msg
      filePath = uriToFilePath $ doc ^. uri
  debug $ "Saved document: " ++ pretty filePath
  tryReCompile stateMVar filePath

onDocumentOpenHandler :: MVar State -> Handlers (LspM ())
onDocumentOpenHandler stateMVar = notificationHandler STextDocumentDidOpen $ \msg -> do
  let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
      filePath = uriToFilePath $ doc ^. uri
  debug $ "Opened document: " ++ pretty filePath
  tryReCompile stateMVar filePath

onDocumentCloseHandler :: MVar State -> Handlers (LspM ())
onDocumentCloseHandler stateMVar = notificationHandler STextDocumentDidClose $ \msg -> debug "Closed document"

onCompletionHandler :: MVar State -> Handlers (LspM ())
onCompletionHandler stateMVar = requestHandler STextDocumentCompletion $ \req responder -> do
  debug "Got completion request"
  let RequestMessage _ _ _ (CompletionParams doc pos _workDone _ _) = req
      completionItem = mkCompletionItem "reduce undefined _ []"
  responder $ Right $ InL $ List [completionItem]

mkCompletionItem :: T.Text -> CompletionItem
mkCompletionItem label = CompletionItem label (Just CiFunction) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
