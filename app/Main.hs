{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom
import Data.JSString
import SocketIO

main :: IO ()
main = do
  mainWidget $ text "Hello, world!"
