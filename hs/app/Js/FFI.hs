{-# LANGUAGE JavaScriptFFI #-}
module Js.FFI where

import Js.Imports
import Js.Types (Url)


newtype Promise a = Promise JSVal

-- general utilities
foreign import javascript unsafe
  "$1.then($2)"
  promise_then :: Promise a -> Callback callback -> IO ()

foreign import javascript unsafe
  "document.addEventListener('keydown', $1)"
  registerOnKeydown :: Callback (JSVal -> IO ()) -> IO ()


-- external dependencies
foreign import javascript unsafe
  "window.downloadReplay($1)"
  downloadReplay :: Url -> IO (Promise Text)


foreign import javascript unsafe
  "window.cachedReplays()"
  cachedReplays :: IO JSArray


foreign import javascript unsafe
  "window.newSocket($1)"
  newSocket :: Url -> IO Object
