{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module GeneralsIO
{--
  ( GameServer(..)
  , Bot(..)
  , newGame
  )
--}
  where

import GHC.Generics (Generic)

import Lens.Micro
import Data.Generics.Labels
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text)
import qualified Data.Aeson as Json

import SocketIO


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
      [ Json.String "set_username"
      , Json.String (bot ^. #id)
      , Json.String (bot ^. #name)
      ]

  pure $ undefined
 -- receive socket

newGame :: NumPlayers -> IO GameServer
newGame numPlayers = do
  uuid <- nextRandom
  pure GameServer { uuid, numPlayers }

join :: SocketIO -> GameServer -> Bot -> IO ()
join = undefined

-- implementation

generalsBotServer :: Url
generalsBotServer = "http://botws.generals.io"
