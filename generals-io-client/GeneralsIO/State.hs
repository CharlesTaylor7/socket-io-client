{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module GeneralsIO.State
  ( GameState(..)
  , GridIndex
  , PlayerId
  , MonadState(..)
  , applyGameStart
  , applyGameUpdate
  )
  where

import GHC.Generics (Generic)
import Control.Monad.State
import Control.Lens
import Data.Generics.Labels ()
import Data.Map (Map)
import GeneralsIO.Events


type GridIndex = (Int, Int)
type PlayerId = Int
data Owner where
  Player  :: PlayerId -> Owner
  Neutral :: Owner
  deriving (Generic, Show, Eq)

data Army = Army
  { owner :: Owner
  , size  :: Int
  }
  deriving (Generic, Show)

data Tile where
  Clear        :: Army -> Tile
  Mountain     :: Tile
  Swamp        :: Army -> Tile
  City         :: Army -> Tile
  General      :: Army -> Tile
  Fog_Clear    :: Tile
  Fog_Obstacle :: Tile
  deriving (Generic, Show)


-- update to include inferences
data GameState = GameState
  { playerId :: PlayerId
  , grid     :: Map GridIndex Tile
  }


applyGameStart :: MonadState GameState m => GameStart -> m ()
applyGameStart event = do
  pure ()

applyGameUpdate :: MonadState GameState m => GameUpdate -> m ()
applyGameUpdate event = do
  pure ()
