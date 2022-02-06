{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.LSP.Server
import Language.LSP.Types

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SInitialized $ \_not -> do
        sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Futhark language extension initialized"),
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover ms (Just range)
            ms = HoverContents $ markedUpContent "futhark-language-server" "Hello world"
            range = Range pos pos
        responder (Right $ Just rsp)
    ]

main :: IO Int
main =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }
