{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.STM
import qualified Data.Aeson as J
import Handlers (onHoverHandler)
import Language.LSP.Server
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger (Priority (DEBUG))
import Utils (debug)

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ -- notificationHandler SInitialized $ \_not -> do
      --   debug "Initialized",
      onHoverHandler
    ]

-- The reactor is a process that serializes and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
newtype ReactorInput = ReactorAction (IO ()) -- State -> IO ()

-- The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debug "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act -- state

-- Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TChan ReactorInput -> Handlers (LspM ())
lspHandlers rin = mapHandlers goReq goNot handlers
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
  state <- newEmptyMVar
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  setupLogger Nothing ["futhark"] DEBUG
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> forkIO (reactor rin) >> pure (Right env),
        staticHandlers = lspHandlers rin,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }
