{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module GeneralsIO.State
  ( GameState(..)
  , GridIndex
  , PlayerId
  , MonadState(..)
  , applyGameUpdate
  )
  where

import Control.Monad.State
import Control.Lens
import Data.Generics.Labels ()

type GridIndex = (Int, Int)
type PlayerId = Int

data GameState = GameState
  { grid     :: Map GridIndex Tile
  , generals :: Map GridIndex PlayerId
  }


applyGameUpdate :: MonadState GameState m => GameUpdate -> m ()
applyGameUpdate event = do
  pure ()
