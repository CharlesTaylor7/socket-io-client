{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (print)
import qualified Prelude

import Control.Monad (forever)
import Control.Concurrent (ThreadId, forkIO)

import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Pipes
import qualified Pipes.Prelude as Pipes

import Control.Concurrent.STM

import qualified SocketIO as Socket
import qualified GeneralsIO.Events as GeneralsIO


main :: IO ()
main = do
  -- connect to the bot server
  (client, output, errors) <- Socket.connect generalsBotServer

  -- send the server events through a channel
  eventChannel <- atomically $ newTBQueue 100

  -- write game events
  _ <- background $
    output >-> Pipes.map (GameEvent . Json.eitherDecode' . BSL.fromStrict) >-> pushToQueue eventChannel

  -- write errors
  _ <- background $
    errors >-> Pipes.map ErrorEvent >-> pushToQueue eventChannel

  let botId = "4321687"
  gameId <- UUID.nextRandom
  Socket.send client $
      [ Json.String "join_private"
      , Json.String (UUID.toText gameId)
      , Json.String botId
      ]

  putStrLn $ "gameid: " <> show gameId

  -- print events to the main thread
  Pipes.runEffect $ Pipes.for (pullFromQueue eventChannel) $ \event -> do
    case event of
      GameEvent (Right generalsEvent) ->
        case generalsEvent of

          _ -> print generalsEvent
      _ -> print event

data SocketOutput
  = ErrorEvent BS.ByteString
  | GameEvent (Either String GeneralsIO.Event)
  deriving Show



print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

background :: Effect IO () -> IO ThreadId
background = forkIO . Pipes.runEffect


pushToQueue :: TBQueue a -> Consumer a IO ()
pushToQueue queue =
  forever $ do
    item <- await
    liftIO $ atomically $ writeTBQueue queue item


pullFromQueue :: TBQueue a -> Producer a IO ()
pullFromQueue queue =
  forever $ do
    item <- liftIO $ atomically $ readTBQueue queue
    yield item


generalsBotServer :: Socket.Url
generalsBotServer = "http://botws.generals.io"
