{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (print)
import qualified Prelude

import Data.Functor (($>))
import Control.Arrow ((|||))
import Control.Monad (forever, when)
import Control.Concurrent (ThreadId, forkIO)

import Data.Generics.Labels ()
import Lens.Micro

import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL

import Pipes
import qualified Pipes.Prelude as Pipes

import qualified SocketIO as Socket

import GeneralsIO.Events (Event(..))
import qualified GeneralsIO.Events as GeneralsIO
import GeneralsIO.Commands
import GeneralsIO.Strategy (basicStrategy)


main :: IO ()
main = botClient


botClient :: forall m. (MonadFail m, MonadIO m) => m ()
botClient = do
  -- connect to the bot server
  (socketEmit, output) <- Socket.connect generalsBotServer

  -- wrap socketEmit
  let
    emitCommands :: Consumer SomeCommand m ()
    emitCommands = socketEmit <-< Pipes.mapM logAndConvert
      where
        logAndConvert :: SomeCommand -> m Json.Array
        logAndConvert (SomeCommand cmd) = do
          print cmd
          pure $ toArgs cmd

  -- write game events
  let
    events =
      output >->
      Pipes.map parseJson >->
      Pipes.mapM (tap print) >->
      Pipes.wither (const (pure Nothing) ||| pure . Just)

    parseJson =  Json.eitherDecode' . BSL.fromStrict


  Pipes.runEffect $
    events >-> basicStrategy >-> emitCommands


-- | Run an effect, and replace the output with the input
tap :: Functor f => (a -> f b) -> (a -> f a)
tap f a = f a $> a

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print


background :: Effect IO () -> IO ThreadId
background = forkIO . Pipes.runEffect


generalsBotServer :: Socket.Url
generalsBotServer = "http://botws.generals.io"
