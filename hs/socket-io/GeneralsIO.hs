{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module GeneralsIO
  ( module SocketIO
  , GameServer(..)
  , Bot(..)
  , Client
  , newGame
  , connect
  , register
  , join
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


connect :: IO (Client, EventStream)
connect = Socket.connect generalsBotServer
  where
    generalsBotServer :: Url
    generalsBotServer = "http://botws.generals.io"


register :: Bot -> Client -> IO ()
register bot socket = do
  send socket $
      [ Json.String "set_username"
      , Json.String (bot ^. #id)
      , Json.String (bot ^. #name)
      ]


newGame :: NumPlayers -> IO GameServer
newGame numPlayers = do
  uuid <- nextRandom
  pure GameServer { uuid, numPlayers }


join :: GameServer -> Bot -> Client -> IO ()
join = undefined
