{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless, forever)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)

import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Aeson as Json
import qualified Pipes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

import qualified SocketIO as Socket
import qualified GeneralsIO.Events as Event


main :: IO ()
main = do
  -- connect to the bot server
  (client, events, errors) <- Socket.connect generalsBotServer

  -- send the server events through a channel
  -- customEventChannel <- newBChan 20
  eventChannel <- atomically $ newTBQueue 100

  forkIO $ Pipes.runEffect $
    Pipes.for events $ \event -> liftIO $ atomically $ do
      writeTBQueue eventChannel (GameEvent event)


  forkIO $ Pipes.runEffect $
    Pipes.for errors $ \event -> liftIO $ atomically $ do
      writeTBQueue eventChannel (ErrorEvent event)


  let botId = "4321687"
  gameId <- UUID.nextRandom
  Socket.send client $
      [ Json.String "join_private"
      , Json.String (UUID.toText gameId)
      , Json.String botId
      ]

  putStrLn $ "gameid: " <> show gameId
  -- print events to the main thread
  forever $ do
    event <- atomically $ readTBQueue eventChannel
    print event


data SocketEvent
  = ErrorEvent Socket.Error
  | GameEvent Socket.Event
  deriving Show

generalsBotServer :: Socket.Url
generalsBotServer = "http://botws.generals.io"


