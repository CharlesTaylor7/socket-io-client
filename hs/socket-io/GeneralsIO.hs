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
  , GameConfig(..)
  , Bot(..)
  , Client
  , newGame
  , connect
  , register
  , join
  )
  where

import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Lens.Micro
import Data.Generics.Labels

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Json

import SocketIO hiding (connect)
import qualified SocketIO as Socket


-- domain models
type NumPlayers = Int

data GameConfig = GameConfig
  { id         :: UUID
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


connect :: MonadIO m => m (Client, EventStream)
connect = liftIO $ Socket.connect generalsBotServer
  where
    generalsBotServer :: Url
    generalsBotServer = "http://botws.generals.io"


register :: MonadIO m => Bot -> Client -> m ()
register bot socket = liftIO $ do
  send socket $
      [ Json.String "set_username"
      , Json.String (bot ^. #id)
      , Json.String (bot ^. #name)
      ]


newGame :: MonadIO m => NumPlayers -> m GameConfig
newGame numPlayers = liftIO $ do
  id <- nextRandom
  pure GameConfig { id, numPlayers }


join :: MonadIO m => GameConfig -> Bot -> Client -> m ()
join config bot client = liftIO $ do
  send client $
      [ Json.String "join_private"
      , Json.String (config ^. #id . to UUID.toText)
      , Json.String (bot ^. #id)
      ]
