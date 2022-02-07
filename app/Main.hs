{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Handlers (onHoverHandler)
import Language.LSP.Server
import Language.LSP.Types (SMethod (SInitialized))
import System.Log.Logger (Priority (DEBUG))
import Utils (debug)

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SInitialized $ \_not -> do
        debug "Initialized",
      onHoverHandler
    ]

main :: IO Int
main = do
  setupLogger Nothing ["futhark"] DEBUG
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> do pure $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }
