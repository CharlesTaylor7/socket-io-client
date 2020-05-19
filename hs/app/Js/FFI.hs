{-# LANGUAGE JavaScriptFFI #-}
module Js.FFI where

import Js.Imports
import Page.Replay.Types (Url)

newtype Promise = Promise JSVal

foreign import javascript unsafe
  "window.downloadReplay($1)"
  downloadReplay :: Url -> IO Promise

foreign import javascript unsafe
  "window.newSocket($1)"
  newSocket :: JSString -> IO Object

foreign import javascript unsafe
  "console.log($1)"
  console_log :: JSVal -> IO ()

foreign import javascript unsafe
  "console.log($1)"
  promise_then :: Promise -> Callback a -> IO ()
