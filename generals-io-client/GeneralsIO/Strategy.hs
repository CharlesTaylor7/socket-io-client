{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module GeneralsIO.Strategy where

import Prelude hiding (print, putStrLn)
import qualified Prelude

import GHC.Generics (Generic)
import Data.Functor (($>))

import Data.Generics.Labels ()
import Control.Lens
import Data.Monoid (First(..))

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.UUID.V4 as UUID
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Pipes
import qualified Pipes.Prelude as Pipes


import GeneralsIO.Events
import GeneralsIO.Commands
import GeneralsIO.State



sendCommand :: (Show cmd, Command cmd, Functor m) => cmd -> Producer' SomeCommand m ()
sendCommand cmd = yield $ SomeCommand cmd

data GameConfig = GameConfig
  { gameId   :: UUID
  , gameSize :: Int
  }
  deriving (Generic)

mkGameConfig :: MonadIO m => Int -> m GameConfig
mkGameConfig gameSize = liftIO $ do
  gameId <- UUID.nextRandom
  pure GameConfig {..}

data Bot = Bot
  { botId   :: Text
  , botName :: Text
  }
  deriving (Generic)

type Behavior m a = Pipe Event SomeCommand m a
type BehaviorConstraints m = (MonadIO m, MonadFail m)
type StrategyConstraints m = (BehaviorConstraints m, MonadState GameState m)


strat :: BehaviorConstraints m => Behavior m ()
strat = do
  let bot = Bot  "4321687" "[Bot] Vorhees"
  gameConfig <- mkGameConfig 2


  gameStart <- joinPrivateGame gameConfig bot
  let runState = flip evalStateT (mkGameState gameStart)

  playGame & hoist runState


playGame :: forall m. StrategyConstraints m => Behavior m ()
playGame = do
  Pipes.for (match #_GameUpdate) $ \e -> do

    chatRoomId <- use $ #gameStart . #chatRoomId
    sendCommand $ Message { chatRoomId, text = "hello" }

joinPrivateGame :: forall m. (BehaviorConstraints m) => GameConfig -> Bot -> Behavior m GameStart
joinPrivateGame gameConfig bot = do
  -- let gameId = gameConfig ^. #gameId . to UUID.toText
  let gameId = "452"
  let gameSize = gameConfig ^. #gameSize
  let botId = bot ^. #botId
  sendCommand $ JoinPrivate {..}
  liftIO $ T.putStrLn $ "http://bot.generals.io/games/" <> gameId

  -- set the game to force start after all players have joined
  let startGame q = (not $ q ^. #isForcing) && q ^. #numPlayers == gameSize
  _ <- matchFirst $ #_QueueUpdate . filtered startGame
  sendCommand $ SetForceStart {force = True, queueId = gameId }

  -- initial info
  matchFirst #_GameStart


-- | match all
match :: Monad m => Getting (First a) s a -> Pipe s a m r
match optic = Pipes.wither (pure . preview optic)


-- | drop until first item in pipe matching traversal
matchFirst :: Functor m => Getting (First a) s a -> Consumer' s m a
matchFirst f = go
  where
    go = do
      s <- await
      case s ^? f of
        Just a  -> pure a
        Nothing -> go



print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.putStrLn
