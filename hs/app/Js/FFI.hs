{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
module Js.FFI where

import Generals.Imports (IO, Int, Double)
import Language.Javascript.JSaddle (JSString, JSVal)

foreign import javascript unsafe "Date.now()"
  now :: IO Double

foreign import javascript unsafe "window.newSocket($1)" newSocket :: JSString -> IO ()

foreign import javascript unsafe "window.downloadReplay($1)" downloadReplay :: JSString -> JSVal

foreign import javascript unsafe "$1 + 3" plus :: Int -> Int
