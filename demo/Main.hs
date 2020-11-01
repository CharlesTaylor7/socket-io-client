{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Aeson as Json
import qualified Pipes
import qualified SocketIO as Socket

import Control.Concurrent (forkIO)


main :: IO ()
main = do
  -- connect to the bot server
  (client, events, errors) <- Socket.connect generalsBotServer

  -- send the server events through the brick side channel
  customEventChannel <- newBChan 20

  forkIO $ Pipes.runEffect $
    Pipes.for errorStream $ \event -> liftIO $ do
      writeBChan customEventChannel (ErrorEvent event)

  forkIO $ Pipes.runEffect $
    Pipes.for eventStream $ \event -> liftIO $ do
      writeBChan customEventChannel (GameEvent event)

  let botId = "43216"
  Socket.send client $
    [ Json.String "set_username"
    , Json.String botId
    , Json.String "asdfqwerty.ew"
    ]

  gameId <- UUID.nextRandom
  Socket.send client $
      [ Json.String "join_private"
      , Json.String "demo-test-543" (UUID.toText gameId)
      , Json.String botId
      ]

  pure ()


data SocketEvent
  = ErrorEvent BS.ByteString
  | NormalEvent Json.Object

generalsBotServer :: Socket.Url
generalsBotServer = "http://botws.generals.io"
