{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
module Js.FFI where

import Js.Imports

foreign import javascript unsafe
  "window.downloadReplay($1, $2)"
  downloadReplay :: JSString -> Callback a -> IO ()

foreign import javascript unsafe
  "window.newSocket($1)"
  newSocket :: JSString -> IO Object
