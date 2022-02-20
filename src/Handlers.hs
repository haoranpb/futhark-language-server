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
onInitializeHandler = notificationHandler SInitialized $ \_not -> debug "Initialized"

onHoverHandler :: MVar State -> Handlers (LspM ())
onHoverHandler state = requestHandler STextDocumentHover $ \req responder -> do
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
      imports <- liftIO $ takeImportsFromState state path
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
