{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import SocketIO
import GeneralsIO

import qualified Data.Aeson as Json
import Data.Foldable
import Data.Traversable


main :: IO ()
main = do
  socket <- connect generalsBotServer
  send socket [Json.String "hey", Json.Number 3]
  vals <- receive socket

  for_ vals $ print
