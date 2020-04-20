{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Reflex.Dom
import Data.JSString

main :: IO ()
main = do
  mainWidget $ text "Hello, world!"
  newSocket ""

foreign import javascript unsafe "import('./js/index.js')" newSocket :: JSString -> IO ()
