{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (print)

import Data.Function ((&))
import Control.Monad.State
import Control.Arrow ((|||))
import Control.Concurrent (ThreadId, forkIO)

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL

import Pipes
import qualified Pipes.Prelude as Pipes

import qualified SocketIO as Socket

import GeneralsIO.Commands (SomeCommand(..), Command(..))
import qualified GeneralsIO.Commands as Cmd

import GeneralsIO.Strategy
import GeneralsIO.State
import GeneralsIO.Events (Event)


main :: IO ()
main = do
  let bot = Bot  "4321687" "[Bot] Vorhees"
  gameConfig <- mkGameConfig 2
  playPrivateGame gameConfig bot
    & botClient
    & flip evalStateT initialGameState


botClient :: forall m. StrategyConstraints m => Strategy m -> m ()
botClient strategy = do
  -- connect to the bot server
  (socketEmit, output) <- Socket.connect generalsBotServer

  -- wrap socketEmit
  let
    emitCommands :: Consumer SomeCommand m ()
    emitCommands = Pipes.chain print >-> Pipes.map Cmd.encode >-> socketEmit

  -- write game events
  let
    events :: Producer Event m ()
    events =
      output >->
      Pipes.map parseJson >->
      Pipes.chain print >->
      Pipes.wither (const (pure Nothing) ||| pure . Just)

    parseJson =  Json.eitherDecode' . BSL.fromStrict


  Pipes.runEffect $
    events >-> strategy >-> emitCommands


background :: Effect IO () -> IO ThreadId
background = forkIO . Pipes.runEffect


generalsBotServer :: Socket.Url
generalsBotServer = "http://botws.generals.io"
