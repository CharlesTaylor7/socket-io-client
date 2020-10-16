module Generals.Replay.Types where

import Data.Aeson (Array(..), FromJSON(..))

import Generals.Map.Types

import Data.Vector

data Server
  = Server_Main
  | Server_Bot
  deriving (Eq, Ord)

data ReplayLocation = ReplayLocation
  { server :: Server
  , id :: Text
  }
  deriving (Eq, Ord, Generic)

type GridLocations = Vector Int

data Replay = Replay
  { id :: Text
  , mapWidth :: Int
  , mapHeight :: Int
  , usernames :: Vector Text

  , cities :: GridLocations
  , cityArmies :: GridLocations
  , generals :: GridLocations
  , mountains :: GridLocations
  , moves :: [Move]

  , afks :: Array
  , teams :: Maybe Array
  , mapTitle :: Maybe Text
  , swamps :: GridLocations
  -- , unknown1 :: Array
  -- , unknown2 :: Array
  }
  deriving (Show, Generic)

data Move = Move
  { playerIndex :: Int
  , startTile :: GridIndex
  , endTile :: GridIndex
  , onlyHalf :: Bool
  , turn :: Int
  }
  deriving (Eq, Show, Generic)

data Command
  = DoNothing
  | Backwards
  | Forwards
  | JumpTo Turn
  deriving (Show)


instance Semigroup Command where
  j@(JumpTo _) <> _     = j
  _ <> j@(JumpTo _)     = j
  anything <> DoNothing = anything
  _ <> latest           = latest


instance Monoid Command where
  mempty = DoNothing

instance Default Command where
  def = DoNothing

newtype Turn = Turn Int
  deriving stock (Show)
  deriving newtype (Ord, Eq, Num)
  deriving (Default, Semigroup, Monoid) via Sum Int


data Perspective
  = Global
  | Perspective Int
  deriving (Show, Eq)

