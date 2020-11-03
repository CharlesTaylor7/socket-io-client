{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module GeneralsIO.Strategy where

import Pipes (Pipe)
import GeneralsIO.Events (Event)
import GeneralsIO.Commands (Command)

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

import Control.Concurrent.STM

import qualified SocketIO as Socket

import GeneralsIO.Events (Event(..))
import qualified GeneralsIO.Events as GeneralsIO
import GeneralsIO.Commands


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
   liftIO $ atomically $ writeTBQueue queue item


type Strategy m = Pipe Event SomeCommand m ()

basicStrategy :: MonadIO m => Strategy m
basicStrategy = do
  -- drive bot
  let botId = "4321687"
  gameId <- UUID.nextRandom
  let gameSize = 2

  putStrLn $ "http://bot.generals.io/games/" <> show gameId

  sendCommand $ JoinPrivate (UUID.toText gameId) botId

  -- have bot respond to events
  Pipes.runEffect $ Pipes.for events $ \event -> liftIO $ do
    case event of
      GameEvent generalsEvent ->
        case generalsEvent of
          QueueUpdate q ->
            when ((not $ q ^. #isForcing) && q ^. #numPlayers == gameSize) $
              sendCommand $ SetForceStart (UUID.toText gameId) True
          _ -> pure ()
      _ -> pure ()



