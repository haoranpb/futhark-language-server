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
  ( onDocumentSaveHandler,
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
      onDocumentSaveHandler stateMVar
    ]

-- The reactor is a process that serializes and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
newtype ReactorInput = ReactorAction (IO ())

-- The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debug "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
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
  debug "init State with Nothing"
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  setupLogger Nothing ["futhark"] DEBUG
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> forkIO (reactor rin) >> pure (Right env),
        staticHandlers = lspHandlers stateMVar rin,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions {textDocumentSync = Just syncOptions}
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
