{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
module Page.Replay.Types where

import Data.Aeson (Array(..), FromJSON(..))
import Data.Default (Default(..))


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
  , _swamps :: [Int]
  -- , _unknown1 :: Array
  -- , _unknown2 :: Array
  }
  deriving (Show)

data Move = Move
  { _playerIndex :: Int
  , _startTile :: GridIndex
  , _endTile :: GridIndex
  , _onlyHalf :: Bool
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


makePrisms ''Server
makePrisms ''Command

makeFieldsNoPrefix ''ReplayLocation
makeFieldsNoPrefix ''Replay
makeFieldsNoPrefix ''Move
