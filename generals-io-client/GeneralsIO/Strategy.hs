{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module GeneralsIO.Strategy where

import Prelude hiding (print, putStrLn)
import qualified Prelude

import GHC.Generics (Generic)
import Data.Functor (($>))
import Control.Arrow ((|||))
import Control.Monad (forever, when)
import Control.Concurrent (ThreadId, forkIO)

import Data.Generics.Labels ()
import Lens.Micro

import Data.Text (Text)
import qualified Data.UUID.V4 as UUID
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL

import Pipes
import qualified Pipes.Prelude as Pipes

import qualified SocketIO as Socket

import GeneralsIO.Events
import GeneralsIO.Protocol hiding (Event(..))


type Strategy phase =
  forall m. MonadIO m =>
  Pipe Event (Command phase) m ()



data GameConfig = GameConfig
  { gameId   :: UUID
  , gameSize :: Int
  }
  deriving (Generic)

mkGameConfig :: Int -> IO GameConfig
mkGameConfig gameSize = do
  gameId <- UUID.nextRandom
  pure GameConfig { gameId, gameSize }

data Bot = Bot
  { botId   :: Text
  , botName :: Text
  }
  deriving (Generic)


joinPrivateGame :: GameConfig -> Bot -> Strategy Connected
joinPrivateGame gameConfig bot = do
  let gameId = gameConfig ^. #gameId . to UUID.toText
  let botId = bot ^. #botId
  sendCommand $ JoinPrivate { gameId, botId }

basicStrategy :: MonadIO m => Strategy m
basicStrategy = do
  -- drive bot
  let botId = "4321687"
  gameId <- liftIO $ UUID.nextRandom
  let gameSize = 2

  putStrLn $ "http://bot.generals.io/games/" <> show gameId

  sendCommand $ JoinPrivate (UUID.toText gameId) botId

  -- have bot respond to events
  Pipes.for Pipes.cat $ \event ->
    case event of
      QueueUpdate q ->
        when ((not $ q ^. #isForcing) && q ^. #numPlayers == gameSize) $
          sendCommand $ SetForceStart (UUID.toText gameId) True
      _ -> pure ()


-- | Run an effect, and replace the output with the input
tap :: Functor f => (a -> f b) -> (a -> f a)
tap f a = f a $> a

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.putStrLn


