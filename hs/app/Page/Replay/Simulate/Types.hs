{-# language TemplateHaskell #-}
module Page.Replay.Simulate.Types where

import Generals.Map.Types hiding (Map)
import Page.Replay.Types hiding (Turn)

import Data.IntSet (IntSet)
import Data.Vector (Vector)

type Turn = (Int, [Move])
type Turns = [Turn]

data Kill = Kill
  { kill_killer :: Int
  , kill_target :: Int
  }

data GameInfo = GameInfo
  { _gameInfo_grid         :: !Grid
  , _gameInfo_activeCities :: !IntSet
  , _gameInfo_activeSwamps :: !IntSet
  , _gameInfo_owned        :: !(IntMap IntSet)
  }

type History = Vector GameInfo


makeLenses ''GameInfo
