{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language NoImplicitPrelude #-}
module Page.Replay.Types where

import Prelude hiding (Show(..))
import Data.Show.Class

import Data.Aeson (Array(..), FromJSON(..))
import Data.Default (Default(..))

import Data.Vector hiding (foldl')

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


instance (Foldable s, Show a) => Show (Zipper' s a) where
  show zip = foldl' (\a b -> a <> ", " <> Prelude.show b) "" (zip & upward & rezip)

type Zipper' s a = Top :>> s a :>> a

data Cache = Cache
  { _cache_zipper :: Zipper' Seq Grid
  , _cache_maxIndex :: Int
  }
  deriving (Show)

type Turn = NonEmpty Move

data Turns = Turns
  { _maxTurn :: Int
  , _lookup :: IntMap Turn
  }

makePrisms ''Server
makePrisms ''Command

makeLenses ''Cache
makeFieldsNoPrefix ''ReplayLocation
makeFieldsNoPrefix ''Replay
makeFieldsNoPrefix ''Move
makeFieldsNoPrefix ''Turns
