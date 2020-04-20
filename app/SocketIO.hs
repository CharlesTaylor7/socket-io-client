{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
module SocketIO where

import Data.JSString

foreign import javascript unsafe "window.newSocket" newSocket :: JSString -> IO ()
