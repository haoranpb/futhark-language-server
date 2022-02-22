{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.STM (atomically)
import qualified Data.Aeson as J
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
import qualified Language.LSP.Types as J
import Language.LSP.Types.Lens (HasSave (save))
import qualified Language.LSP.Types.Lens as J
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

-- Reactor design copied from lsp/Reactor example
-- Not sure if needed
newtype ReactorInput = ReactorAction (IO ())

reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debug "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

lspHandlers :: MVar State -> TChan ReactorInput -> Handlers (LspM ())
lspHandlers stateMVar rin = mapHandlers goReq goNot (handlers stateMVar)
  where
    goReq :: forall (a :: J.Method J.FromClient J.Request). Handler (LspM ()) a -> Handler (LspM ()) a
    goReq f = \msg k -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

    goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler (LspM ()) a -> Handler (LspM ()) a
    goNot f = \msg -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

main :: IO Int
main = do
  stateMVar <- newMVar emptyState
  debug "Init with emptyState"
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  setupLogger Nothing ["futhark"] DEBUG
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> forkIO (reactor rin) >> pure (Right env),
        staticHandlers = lspHandlers stateMVar rin,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options =
          defaultOptions
            { textDocumentSync = Just syncOptions,
              completionTriggerCharacters = Just ['.']
            }
      }

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Nothing, -- Just TdSyncIncremental,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just $ InR $ SaveOptions $ Just False
    }
