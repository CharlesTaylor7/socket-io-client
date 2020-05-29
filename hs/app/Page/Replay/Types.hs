{-# language TemplateHaskell #-}
module Page.Replay.Types where

import Data.Aeson (Array(..), FromJSON(..))
import Data.Default (Default(..))

import Types (Dimensions(..))
import Generals.Map.Types hiding (Map)

import Data.Vector

data Server
  = Server_Main
  | Server_Bot

data ReplayLocation = ReplayLocation
  { _replayLocation_server :: Server
  , _replayLocation_id :: Text
  }

type GridLocations = Vector Int

data Replay = Replay
  { _replay_id :: Text
  , _replay_mapWidth :: Int
  , _replay_mapHeight :: Int
  , _replay_usernames :: Array

  , _replay_cities :: GridLocations
  , _replay_cityArmies :: GridLocations
  , _replay_generals :: GridLocations
  , _replay_mountains :: GridLocations
  , _replay_moves :: [Move]

  , _replay_afks :: Array
  , _replay_teams :: Maybe Array
  , _replay_mapTitle :: Maybe Text
  , _replay_swamps :: GridLocations
  -- , _replay_unknown1 :: Array
  -- , _replay_unknown2 :: Array
  }
  deriving (Show)

data Move = Move
  { _move_playerIndex :: Int
  , _move_startTile :: GridIndex
  , _move_endTile :: GridIndex
  , _move_onlyHalf :: Bool
  , _move_turn :: Int
  }
  deriving (Eq, Show, Generic)

data Command
  = Backwards
  | Forwards
  | JumpTo Int
  deriving (Show)

instance Semigroup Command where
  JumpTo n <> _replay_ = JumpTo n
  _ <> JumpTo n = JumpTo n
  _ <> latest   = latest


makePrisms ''Server
makePrisms ''Command

makeLenses ''ReplayLocation
makeLenses ''Replay
makeLenses ''Move
