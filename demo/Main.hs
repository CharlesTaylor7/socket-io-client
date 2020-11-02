{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Prelude hiding (print)
import qualified Prelude

import Control.Arrow ((|||))
import Control.Monad (forever, when)
import Control.Concurrent (ThreadId, forkIO)
import System.Exit (ExitCode(..), exitWith)

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


main :: IO ()
main = do
  -- connect to the bot server
  (socketEmit, output, errors, exitCode) <- Socket.connect generalsBotServer

  -- send the server events through a channel
  eventChannel <- atomically $ newTBQueue 100

  -- write game events
  _ <- background $
    output >-> Pipes.map ((ParserError ||| GameEvent) . Json.eitherDecode' . BSL.fromStrict) >-> pushToQueue eventChannel

  -- write errors
  _ <- background $
    errors >-> Pipes.map ClientErrorLine >-> pushToQueue eventChannel

  _ <- background $
    exitCode >-> Pipes.map ClientDied >-> pushToQueue eventChannel

  let botId = "4321687"
  gameId <- UUID.nextRandom
  let gameSize = 2

  liftIO $ socketEmit
      [ Json.String "join_private"
      , Json.String (UUID.toText gameId)
      , Json.String botId
      ]

  putStrLn $ "gameid: " <> show gameId

  -- print events to the main thread
  Pipes.runEffect $ Pipes.for (pullFromQueue eventChannel) $ \event -> liftIO $ do
    case event of
      ClientDied exitCode -> do
        putStrLn $ "socket.io client process exited with " <> show exitCode
        exitWith (ExitFailure 1)
      GameEvent generalsEvent ->
        case generalsEvent of
          QueueUpdate q ->
            when ((not $ q ^. #isForcing) && q ^. #numForce == gameSize) $
              socketEmit
                [ Json.String "set_force_start"
                , Json.String (UUID.toText gameId)
                , Json.Bool True
                ]
          _ -> print generalsEvent
      _ -> print event


data SocketOutput
  = GameEvent GeneralsIO.Event
  -- ^ generals event
  | ParserError String
  -- ^ error parsing generals event
  | ClientErrorLine BS.ByteString
  -- ^ line from stderr of socket client
  | ClientDied ExitCode
  -- ^ exit code from stderr of socket client
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
