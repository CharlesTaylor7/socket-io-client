{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
module SocketIO where

import Generals.Imports (IO)
import Language.Javascript.JSaddle

foreign import javascript unsafe "window.newSocket" newSocket :: JSString -> IO ()
