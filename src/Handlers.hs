{-# LANGUAGE OverloadedStrings #-}

module Handlers (handlers) where

import Compile (tryReCompile, tryTakeStateFromMVar)
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Futhark.Util.Pretty (pretty)
import Language.LSP.Server (Handlers, LspM, getVersionedTextDoc, notificationHandler, requestHandler)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri), HasVersion (version))
import SemanticTokens (getSemanticTokens)
import Tool (getHoverInfoFromState)
import Utils (State (..), debug)

handlers :: MVar State -> Handlers (LspM ())
handlers stateMVar =
  mconcat
    [ onInitializeHandler,
      onHoverHandler stateMVar,
      onDocumentOpenHandler stateMVar,
      onDocumentCloseHandler,
      onDocumentSaveHandler stateMVar,
      onCompletionHandler stateMVar,
      onSemanticTokensHandler stateMVar
    ]

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
  versionedDoc <- getVersionedTextDoc doc
  debug $ "Saved document: " ++ show versionedDoc
  tryReCompile stateMVar filePath (versionedDoc ^. version)

onDocumentOpenHandler :: MVar State -> Handlers (LspM ())
onDocumentOpenHandler stateMVar = notificationHandler STextDocumentDidOpen $ \msg -> do
  let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
      filePath = uriToFilePath $ doc ^. uri
  debug $ "Opened document: " ++ pretty filePath
  tryReCompile stateMVar filePath (Just $ doc ^. version)

onDocumentCloseHandler :: Handlers (LspM ())
onDocumentCloseHandler = notificationHandler STextDocumentDidClose $ \_msg -> debug "Closed document"

onSemanticTokensHandler :: MVar State -> Handlers (LspM ())
onSemanticTokensHandler stateMVar = requestHandler STextDocumentSemanticTokensFull $ \req responder -> do
  debug "Got semantic tokens request"
  let RequestMessage _ _ _ (SemanticTokensParams _workDone _partialToken doc) = req
      filePath = uriToFilePath $ doc ^. uri
  state <- tryTakeStateFromMVar stateMVar filePath
  tokens <- liftIO $ getSemanticTokens state filePath
  responder $ Right $ Just tokens

onCompletionHandler :: MVar State -> Handlers (LspM ())
onCompletionHandler _stateMVar = requestHandler STextDocumentCompletion $ \req responder -> do
  debug "Got completion request"
  let RequestMessage _ _ _ (CompletionParams _doc _pos _workDone _ _) = req
      completionItem = mkCompletionItem "reduce undefined _ []"
  responder $ Right $ InL $ List [completionItem]

-- TODO: separate completion logic to another file
mkCompletionItem :: T.Text -> CompletionItem
mkCompletionItem label = CompletionItem label (Just CiFunction) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
