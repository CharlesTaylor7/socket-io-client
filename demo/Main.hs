{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Aeson as Json
import qualified Pipes
import qualified SocketIO as Socket
import qualified Data.ByteString as BS

import Control.Concurrent (forkIO)


main :: IO ()
main = do
  -- connect to the bot server
  (client, events, errors) <- Socket.connect generalsBotServer

  -- send the server events through a channel
  -- customEventChannel <- newBChan 20

  forkIO $ Pipes.runEffect $
    Pipes.for errors $ \event -> liftIO $ do
      print event
      -- writeBChan customEventChannel (ErrorEvent event)

  forkIO $ Pipes.runEffect $
    Pipes.for events $ \event -> liftIO $ do
      print event
      -- writeBChan customEventChannel (GameEvent event)

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

  -- keep main thread alive
  forever $ pure ()


data SocketEvent
  = ErrorEvent BS.ByteString
  | GameEvent Json.Object

generalsBotServer :: Socket.Url
generalsBotServer = "http://botws.generals.io"
