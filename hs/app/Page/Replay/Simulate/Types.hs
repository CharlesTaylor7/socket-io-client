module Page.Replay.Simulate.Types where

import Generals.Map.Types hiding (Map)
import Page.Replay.Types hiding (Turn)

import Data.IntSet (IntSet)
import Data.Vector (Vector)

type Turn = (Int, [Move])
type Turns = [Turn]

type History = Vector GameInfo

data Kill = Kill
  { killer :: Int
  , target :: Int
  }
  deriving (Show, Eq)

data GameInfo = GameInfo
  { grid         :: !Grid
  , activeCities :: !IntSet
  , activeSwamps :: !IntSet
  , owned        :: !(IntMap IntSet)
  -- kills, in reverse chronological order
  , kills        :: ![Kill]
  -- get only
  , numTiles     :: !Int
  , replay       :: !Replay
  }
