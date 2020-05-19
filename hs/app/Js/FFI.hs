{-# LANGUAGE JavaScriptFFI #-}
module Js.FFI where

import Js.Imports
import Page.Replay.Types (Url)

foreign import javascript unsafe
  "window.downloadReplay($1, $2)"
  downloadReplay :: Url -> Callback a -> IO ()

foreign import javascript unsafe
  "window.newSocket($1)"
  newSocket :: JSString -> IO Object
