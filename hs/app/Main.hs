{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where

import Generals.Imports

import Page.Replay

main :: IO ()
main = mainWidget $ do
  text "hello world!"
  replay
