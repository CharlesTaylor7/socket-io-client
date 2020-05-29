{-# language TemplateHaskell #-}
module Page.Replay.Simulate.Types where

import Generals.Map.Types hiding (Map)
import Page.Replay.Types

import Data.IntSet (IntSet)
import Data.Vector (Vector)

type Turn = (Int, [Move])
type Turns = [Turn]
type GameInfo = (Grid, Cache)

data Kill = Kill
  { kill_killer :: Int
  , kill_target :: Int
  }

data Cache = Cache
  { _cache_activeCities :: IntSet
  , _cache_activeSwamps :: IntSet
  , _cache_owned :: Vector IntSet
  }

makeLenses ''Cache
