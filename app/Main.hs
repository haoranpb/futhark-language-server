{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Handlers
  ( onCompletionHandler,
    onDocumentCloseHandler,
    onDocumentOpenHandler,
    onDocumentSaveHandler,
    onHoverHandler,
    onInitializeHandler,
  )
import Language.LSP.Server
import Language.LSP.Types
import System.Log.Logger (Priority (DEBUG))
import Utils (State (State), debug, emptyState)

handlers :: MVar State -> Handlers (LspM ())
handlers stateMVar =
  mconcat
    [ onInitializeHandler,
      onHoverHandler stateMVar,
      onDocumentOpenHandler stateMVar,
      onDocumentCloseHandler stateMVar,
      onDocumentSaveHandler stateMVar,
      onCompletionHandler stateMVar
    ]

main :: IO Int
main = do
  stateMVar <- newMVar emptyState
  debug "Init with emptyState"
  setupLogger Nothing ["futhark"] DEBUG
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> do pure $ Right env,
        staticHandlers = handlers stateMVar,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options =
          defaultOptions
            { textDocumentSync = Just syncOptions
            }
      }

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Just TdSyncIncremental,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just $ InR $ SaveOptions $ Just False
    }
