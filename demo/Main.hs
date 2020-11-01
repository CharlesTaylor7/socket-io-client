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
import qualified SocketIO as Socket
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue


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


  let botId = "43216"
  Socket.send client $
    [ Json.String "set_username"
    , Json.String botId
    , Json.String "asdfqwerty.ew"
    ]

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
  = ErrorEvent BS.ByteString
  | GameEvent Json.Object
  deriving Show

generalsBotServer :: Socket.Url
generalsBotServer = "http://botws.generals.io"
