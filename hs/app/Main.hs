{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where

import Generals.Imports
import SocketIO

import Page.Replay

main :: IO ()
main = mainWidget $ do
  text "Hello, world!"
  replay
