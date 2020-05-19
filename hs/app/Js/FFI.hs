{-# LANGUAGE JavaScriptFFI #-}
module Js.FFI where

import Js.Imports
import Page.Replay.Types (Url)

newtype Promise a = Promise JSVal

-- general utilities
foreign import javascript unsafe
  "$1.then($2)"
  promise_then :: Promise a -> Callback callback -> IO ()

foreign import javascript unsafe
  "console.log($1)"
  console_log :: JSVal -> IO ()

foreign import javascript unsafe
  "fetch($1).then(function(body) { return body.text() })"
  fetch_bodytext :: Url -> IO (Promise Text)

-- external dependencies
foreign import javascript unsafe
  "window.downloadReplay($1)"
  downloadReplay :: Url -> IO (Promise Text)

foreign import javascript unsafe
  "window.newSocket($1)"
  newSocket :: Url -> IO Object
