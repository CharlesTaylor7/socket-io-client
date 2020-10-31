{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
module GeneralsIO
  ( GameServer(..)
  , Bot(..)
  , newGame
  )
  where

import SocketIO
import Lens.Micro
import Data.Generics.Labels
import GHC.Generics (Generic)

-- domain model
type NumPlayers = Int

data GameServer = GameServer
  { uuid       :: UUID
  , numPlayers :: Int
  }
  deriving (Generic)

data UnregisteredBot = UnregisteredBot
  { id         :: Text
  , name       :: Text
  }
  deriving (Generic)

data Bot = Bot
  { id         :: Text
  , name       :: Text
  , registered :: Bool
  }
  deriving (Generic)

data RegistrationError
  = BotIsAlreadyRegistered
  | BotNameIsTaken
  | BotNameMustStartWithBot

register :: SocketIO -> UnregisteredBot -> IO (Either RegistrationError Bot)
register socket bot = do
  send socket $
    Json.Array
      [ String "set_username"
      , String (bot ^. #id)
      , String (bot ^. #name)
      ]

 -- receive socket

newGame :: NumPlayers -> IO GameServer
newGame numPlayers = do
  uuid <- nextRandom
  pure GameServer { uuid, numPlayers }

join :: SocketIO -> GameServer -> Bot -> IO ()
join = undefined

-- implementation

generalsBotServer :: Text
generalsBotServer = "http://botws.generals.io"
