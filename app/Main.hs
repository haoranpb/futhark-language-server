{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module Main where

import Control.Concurrent (forkIO, newEmptyMVar)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.STM (atomically)
import qualified Data.Aeson as J
import Handlers (onHoverHandler, onInitializeHandler)
import Language.LSP.Server
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.Log.Logger (Priority (DEBUG))
import Utils (State (State), debug)

handlers :: MVar State -> Handlers (LspM ())
handlers state =
  mconcat
    [ onInitializeHandler,
      onHoverHandler state
    ]

-- The reactor is a process that serializes and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
newtype ReactorInput = ReactorAction (IO ()) -- State -> IO ()

-- The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: MVar State -> TChan ReactorInput -> IO ()
reactor state inp = do
  debug "Started the reactor"
  forever $ do
    -- s <- tryTakeMVar state
    -- -- check if state is empty, if so, compile
    -- case s of
    --   Nothing -> debug "State is empty, compiling" -- how to get the filePath to compile?
    --   Just _st -> debug "State exits, continuing" -- check if needed re-compile
    ReactorAction act <- atomically $ readTChan inp
    act

-- Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: MVar State -> TChan ReactorInput -> Handlers (LspM ())
lspHandlers state rin = mapHandlers goReq goNot (handlers state)
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
  state <- newMVar (State Nothing)
  debug "init State with Nothing"
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  setupLogger Nothing ["futhark"] DEBUG
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> forkIO (reactor state rin) >> pure (Right env),
        staticHandlers = lspHandlers state rin,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }
