{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Futhark.Compiler (readProgramOrDie)
import Futhark.Util.Loc (Pos (Pos), srclocOf)
import Language.Futhark.Query
import Language.Futhark.Syntax (locStr, pretty)
import Language.LSP.Server (Handlers, LspM, notificationHandler, requestHandler)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Utils (debug)

onHoverHandler :: Handlers (LspM ())
onHoverHandler = requestHandler STextDocumentHover $ \req responder -> do
  debug "Got hover request"
  let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      Position l c = pos
      rsp = Hover ms (Just range)
      ms = HoverContents $ markedUpContent "futhark-language-server" "Hello world"
      range = Range pos pos
  let filePath = uriToFilePath $ doc ^. uri
  case filePath of
    Just path -> do
      debug $ show path
      debug $ "LSP Position: " ++ show (l, c)
      (_, imports, _) <- readProgramOrDie path
      case atPos imports $ Pos path (fromEnum l + 1) (fromEnum c) 0 of
        Nothing -> debug "No information available"
        Just (AtName qn def loc) -> do
          debug $ "Name: " ++ show qn
          debug $ "Position: " ++ locStr (srclocOf loc)
          case def of
            Nothing -> return ()
            Just (BoundTerm t defloc) -> do
              debug $ "Type: " ++ pretty t
              debug $ "Definition: " ++ locStr (srclocOf defloc)
            Just (BoundType defloc) ->
              debug $ "Definition: " ++ locStr (srclocOf defloc)
            Just (BoundModule defloc) ->
              debug $ "Definition: " ++ locStr (srclocOf defloc)
            Just (BoundModuleType defloc) ->
              debug $ "Definition: " ++ locStr (srclocOf defloc)
    Nothing -> do
      debug "No path"
  responder (Right $ Just rsp)

-- onOpenDocumentHandler :: MVar State -> Handlers (LspM ())
-- onOpenDocumentHandler state = notificationHandler STextDocumentDidOpen $ \msg -> do
--   let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
--       filePath = uriToFilePath $ doc ^. uri
--   case filePath of
--     Just path -> do
--       debug $ "On open file" ++ show path
--       debug "Compiling..."
--       (_, imports, _) <- readProgramOrDie path
--       liftIO $ putMVar state $ State imports
--       debug "Compilation done"
--     Nothing -> do
--       debug "404 file not found"
