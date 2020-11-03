{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Pipes
import qualified Pipes.Prelude as Pipes

import Control.Concurrent.STM

import qualified SocketIO as Socket

import GeneralsIO.Events (Event(..))
import qualified GeneralsIO.Events as GeneralsIO
import GeneralsIO.Commands


main :: IO ()
main = do
  -- connect to the bot server
  (socketEmit, output) <- Socket.connect generalsBotServer

  -- send the server events through a channel
  eventChannel <- atomically $ newTBQueue 100

  -- write game events
  _ <- background $
    output >-> Pipes.map ((ParseError ||| GameEvent) . Json.eitherDecode' . BSL.fromStrict) >-> pushToQueue eventChannel

  let botId = "4321687"
  gameId <- UUID.nextRandom
  let gameSize = 2

  let
    sendCommand :: (MonadIO m, Command cmd) => cmd -> m ()
    sendCommand cmd = liftIO $ do
        print cmd
        socketEmit $ toArgs cmd

  putStrLn $ "http://bot.generals.io/games/" <> show gameId

  sendCommand $ JoinPrivate (UUID.toText gameId) botId
  sendCommand $ Cancel
  sendCommand $ StarsAndRank botId

  -- log all events to the main thread
  let pipeline = pullFromQueue eventChannel >-> Pipes.mapM (tap print)
  Pipes.runEffect $ Pipes.for pipeline $ \event -> liftIO $ do
    case event of
      GameEvent generalsEvent ->
        case generalsEvent of
          QueueUpdate q ->
            when ((not $ q ^. #isForcing) && q ^. #numPlayers == gameSize) $
              sendCommand $ SetForceStart (UUID.toText gameId) True
          _ -> pure ()
      _ -> pure ()

-- | Run an effect, and replace the output with the input
tap :: Functor f => (a -> f b) -> (a -> f a)
tap f a = f a $> a

data SocketOutput
  = GameEvent GeneralsIO.Event
  -- ^ generals event
  | ParseError String
  -- ^ error parsing generals event
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
