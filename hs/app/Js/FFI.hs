{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
module Js.FFI where

import Generals.Imports (IO)
import Language.Javascript.JSaddle (JSString)

foreign import javascript unsafe "window.newSocket" newSocket :: JSString -> IO ()
foreign import javascript unsafe "window.downloadReplay" downloadReplay :: JSString -> IO JSString
