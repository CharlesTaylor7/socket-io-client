{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Data.JSString


foreign import javascript unsafe "import('./js/index.js')" newSocket :: JSString -> IO ()

main = newSocket ""
