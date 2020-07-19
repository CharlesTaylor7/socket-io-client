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
  -- get only
  , _gameInfo_gridWidth    :: !Int
  , _gameInfo_numTiles     :: !Int
  }

type History = Vector GameInfo

gameInfo_gridWidth :: Getter GameInfo Int
gameInfo_gridWidth = to _gameInfo_gridWidth

gameInfo_numTiles :: Getter GameInfo Int
gameInfo_numTiles = to _gameInfo_numTiles

makeLensesFor
  [ ("_gameInfo_grid", "gameInfo_grid")
  , ("_gameInfo_activeCities", "gameInfo_activeCities")
  , ("_gameInfo_activeSwamps", "gameInfo_activeSwamps")
  , ("_gameInfo_owned", "gameInfo_owned")
  ]
  ''GameInfo
