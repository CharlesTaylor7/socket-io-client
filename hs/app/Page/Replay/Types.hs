{-# LANGUAGE NamedFieldPuns #-}
module Page.Replay.Types where

import Types (Dimensions(..))
import Data.Aeson (Array(..), FromJSON(..))
import Data.Default (Default(..))

import Data.Vector


data Server
  = Server_Main
  | Server_Bot

data ReplayLocation = ReplayLocation
  { server :: Server
  , replay_id :: Text
  }

data Move = Move
  { playerIndex :: Int
  , startTileIndex :: Int
  , endTileIndex :: Int
  , is50 :: Bool
  , turn :: Int
  }
  deriving stock (Eq, Show, Generic)


data Replay = Replay
  { id :: Text
  , dimensions :: Dimensions
  , usernames :: Array

  , cities :: [Int]
  , cityArmies :: [Int]
  , generals :: [Int]
  , mountains :: [Int]
  , moves :: Vector Move

  , afks :: Array
  , teams :: Maybe Array
  , mapTitle :: Maybe Text
  }
  deriving (Eq, Show)
