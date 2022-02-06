{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.IO.Class
import qualified Data.Text as T
import Futhark.Compiler (readProgramOrDie)
import Futhark.Util.Loc
import Language.Futhark.Query
import Language.Futhark.Syntax (locStr, pretty)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import System.Log.Logger

debug :: String -> LspT () IO ()
debug msg = liftIO $ debugM "futhark" msg

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SInitialized $ \_not -> do
        liftIO $ debugM "futhark" "Initialized",
      requestHandler STextDocumentHover $ \req responder -> do
        liftIO $ debugM "futhark" "Got hover request"
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
    ]

main :: IO Int
main = do
  setupLogger Nothing ["futhark"] DEBUG
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> do
          liftIO $ debugM "futhark" "Initializing"
          -- ? is it the right pos to first compile the file
          -- ? where to store the result
          pure $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }
