{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import SocketIO
import GeneralsIO

import qualified Data.Aeson as Json


main :: IO ()
main = do
  socket <- connect generalsBotServer
  send socket [Json.String "hey", Json.Number 3]
  receive socket
