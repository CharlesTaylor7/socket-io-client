{-# LANGUAGE NamedFieldPuns #-}
module Page.Replay.Types where

import Types (Dimensions(..))
import Data.Aeson (Array(..))

data Server
  = Server_Main
  | Server_Bot

data ReplayLocation = ReplayLocation
  { server :: Server
  , replay_id :: Text
  }

newtype Url = Url Text

data Replay = Replay
  { id :: Text
  , dimensions :: Dimensions
  , usernames :: Array
  , cities :: Array
  , cityArmies :: Array
  , generals :: Array
  , mountains :: Array
  , moves :: Array
  , afks :: Array
  , teams :: Maybe Array
  , mapTitle :: Maybe Text
  }
  deriving (Eq, Show)
