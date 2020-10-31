{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module GeneralsIO
  ( GameServer(..)
  , Bot(..)
  , newGame
  , module SocketIO
  )
  where

import GHC.Generics (Generic)

import Lens.Micro
import Data.Generics.Labels
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text)
import qualified Data.Aeson as Json

import SocketIO hiding (connect)
import qualified SocketIO as Socket


-- domain models
type NumPlayers = Int

type EventStream = [Json.Value]

data GameServer = GameServer
  { uuid       :: UUID
  , numPlayers :: Int
  }
  deriving (Generic)

data Bot = Bot
  { id         :: Text
  , name       :: Text
  , registered :: Bool
  }
  deriving (Generic)
  deriving anyclass (Json.FromJSON)


connect :: IO SocketIO
connect = Socket.connect generalsBotServer
  where
    generalsBotServer :: Url
    generalsBotServer = "http://botws.generals.io"

register :: SocketIO -> UnregisteredBot -> IO ()
register socket bot = do
  send socket $
      [ Json.String "set_username"
      , Json.String (bot ^. #id)
      , Json.String (bot ^. #name)
      ]


newGame :: NumPlayers -> IO GameServer
newGame numPlayers = do
  uuid <- nextRandom
  pure GameServer { uuid, numPlayers }

join :: SocketIO -> GameServer -> Bot -> IO ()
join = undefined
