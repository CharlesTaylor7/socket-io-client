{-# LANGUAGE NamedFieldPuns #-}
module Page.Replay.Types where

import Types (Dimensions(..))
import Data.Aeson (Array(..))
import Data.Default (Default(..))

import Data.Vector


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
  , generals :: Vector Int
  , mountains :: [Int]

  , moves :: Array
  , afks :: Array
  , teams :: Maybe Array
  , mapTitle :: Maybe Text
  }
  deriving (Eq, Show)
