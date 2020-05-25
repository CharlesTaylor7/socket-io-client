{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}

module Page.Replay.Types where

import Data.Aeson (Array(..), FromJSON(..))
import Data.Default (Default(..))

import Data.Vector

import Types (Dimensions(..))
import Generals.Map.Types hiding (Map)


data Server
  = Server_Main
  | Server_Bot

data ReplayLocation = ReplayLocation
  { _server :: Server
  , _id :: Text
  }


data Replay = Replay
  { _id :: Text
  , _mapWidth :: Int
  , _mapHeight :: Int
  , _usernames :: Array

  , _cities :: [Int]
  , _cityArmies :: [Int]
  , _generals :: [Int]
  , _mountains :: [Int]
  , _moves :: [Move]

  , _afks :: Array
  , _teams :: Maybe Array
  , _mapTitle :: Maybe Text
  }
  deriving (Show)

data Move = Move
  { _playerIndex :: Int
  , _startTileIndex :: Int
  , _endTileIndex :: Int
  , _is50 :: Bool
  , _turn :: Int
  }
  deriving (Eq, Show, Generic)

data Command
  = Backwards
  | Forwards
  | JumpTo Int
  deriving (Show)

instance Semigroup Command where
  JumpTo n <> _ = JumpTo n
  _ <> JumpTo n = JumpTo n
  _ <> latest   = latest

data Cache = Cache
  { _currentIndex :: Int
  , _history :: Seq Grid
  }
  deriving (Show)

data Move' = Move'
  { _startTile :: GridIndex
  , _endTile   :: GridIndex
  , _onlyHalf  :: Bool
  }

type Turn = NonEmpty Move'

data Turns = Turns
  { _maxTurn :: Int
  , _lookup :: Map Int Turn
  }

makePrisms ''Server
makePrisms ''Command

makeFieldsNoPrefix ''ReplayLocation
makeFieldsNoPrefix ''Replay
makeFieldsNoPrefix ''Cache
makeFieldsNoPrefix ''Move
makeFieldsNoPrefix ''Move'
makeFieldsNoPrefix ''Turns
