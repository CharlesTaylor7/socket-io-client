{-# language TemplateHaskell #-}
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
  { server :: Server
  , replay_id :: Text
  }


data Replay = Replay
  { id :: Text
  , dimensions :: Dimensions
  , usernames :: Array

  , cities :: [Int]
  , cityArmies :: [Int]
  , generals :: [Int]
  , mountains :: [Int]
  , moves :: [Move]

  , afks :: Array
  , teams :: Maybe Array
  , mapTitle :: Maybe Text
  }
  deriving (Eq, Show)

-- type Grid = Containers.Map (Int, Int) Tile

data Move = Move
  { playerIndex :: Int
  , startTileIndex :: Int
  , endTileIndex :: Int
  , is50 :: Bool
  , turn :: Int
  }
  deriving (Eq, Show, Generic)

data Command
  = Backwards
  | Forwards
  deriving (Show)


data Cache = Cache
  { _currentIndex :: Int
  , _history :: Map Int Grid
  }
  deriving (Show)

data Move' = Move'
  { startTile :: (Int, Int)
  , endTile   :: (Int, Int)
  , onlyHalf  :: Bool
  }

type Turn = NonEmpty Move'

data Turns = Turns
  { maxTurn :: Int
  , turnsMap :: Map Int Turn
  }


makeLenses ''Cache
