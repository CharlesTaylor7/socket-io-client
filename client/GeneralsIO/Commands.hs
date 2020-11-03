{-# Language DuplicateRecordFields #-}
{-# Language OverloadedLists #-}
{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
{-# Language DerivingStrategies #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingAnyClass #-}

module GeneralsIO.Commands where

import GHC.Generics (Generic)

import Data.Functor
import Data.Foldable (asum)
import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector, (!?))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Aeson
import qualified Data.Aeson.Types as Json

import Debug.Trace (traceShow)


class Command where
  toArgs :: Json.Array


-- | leave_game
-- Leave game
data LeaveGame = LeaveGame
  deriving (Show, Generic)

instance Command LeaveGame where
  toArgs LeaveGame = Json.Array [ Json.String "leave_game" ]


-- | set_username
-- Register username to bot
data SetUsername = SetUsername
  { userId   :: Text
  , username :: Text
  }
  deriving (Show, Generic)

instance Command SetUsername where
  toArgs cmd = Json.Array
    [ Json.String "set_username"
    , Json.String (userId cmd)
    , Json.String (username cmd)
    ]

-- | play
-- Join FFA queue
data Play = Play
  { userId   :: Text
  }
  deriving (Show, Generic)

instance Command Play where
  toArgs cmd = Json.Array
    [ Json.String "play"
    , Json.String (userId cmd)
    ]

-- | join_1v1
-- Join 1v1 queue
data Join1v1 = Join1v1
  { userId   :: Text
  }
  deriving (Show, Generic)

instance Command Join1v1 where
  toArgs cmd = Json.Array
    [ Json.String "join_1v1"
    , Json.String (userId cmd)
    ]

-- | join_private
-- Join a private game
data JoinPrivate = JoinPrivate
  { customGameId :: Text
  , userId       :: Text
  }
  deriving (Show, Generic)

instance Command JoinPrivate where
  toArgs cmd = Json.Array
    [ Json.String "join_private"
    , Json.String (customGameId cmd)
    , Json.String (userId cmd)
    ]

-- | set_custom_team
-- Choose team number in a custom game
data SetCustomTeam = SetCustomTeam
  { customGameId :: Text
  , team         :: Int -- ^ between 1 & 8 inclusive
  }
  deriving (Show, Generic)

instance Command SetCustomTeam where
  toArgs cmd = Json.Array
    [ Json.String "set_custom_team"
    , Json.String (customGameId cmd)
    , Json.String (team cmd)
    ]

-- | join_team
-- join a team in the 2v2 queue
data JoinTeam = JoinTeam
  { teamId :: Text
  , userId :: Int
  }
  deriving (Show, Generic)

instance Command JoinTeam where
  toArgs cmd = Json.Array
    [ Json.String "join_team"
    , Json.String (teamId cmd)
    , Json.String (userId cmd)
    ]

-- | leave_team
-- leave a team in the 2v2 queue
data LeaveTeam = LeaveTeam
  { teamId :: Text
  }
  deriving (Show, Generic)

instance Command LeaveTeam where
  toArgs cmd = Json.Array
    [ Json.String "leave_team"
    , Json.String (teamId cmd)
    ]

-- | cancel
-- leave the current game queue
data Cancel = Cancel
  deriving (Show, Generic)

instance Command Cancel where
  toArgs cmd = Json.Array
    [ Json.String "cancel"
    ]

-- | set_force_start
-- Toggle force start status in multiplayer game
data SetForceStart = SetForceStart
  { queueId :: Text
  , doForce :: Bool
  }
  deriving (Show, Generic)

instance Command SetForceStart where
  toArgs cmd = Json.Array
    [ Json.String "set_force_start"
    , Json.String (queueId cmd)
    , Json.String (doForce cmd)
    ]

-- | attack
-- Make a move in game
data Attack = Attack
  { start    :: Int
  , end      :: Int
  , onlyHalf :: Bool
  }
  deriving (Show, Generic)

instance Command SetForceStart where
  toArgs cmd = Json.Array
    [ Json.String "attack"
    , Json.String (start cmd)
    , Json.String (end cmd)
    , Json.String (onlyHalf cmd)
    ]

-- | clear_moves
-- Clear move queue
data ClearMoves = ClearMoves
  deriving (Show, Generic)

instance Command ClearMoves where
  toArgs cmd = Json.Array
    [ Json.String "clear_moves"
    ]

-- | ping_tile
-- Signals a tile to all teammates in a team game
data PingTile = PingTile
  { tile  :: Int
  }
  deriving (Show, Generic)

instance Command PingTile where
  toArgs cmd = Json.Array
    [ Json.String "ping_tile"
    , Json.Number (tile cmd)
    ]

-- | chat_message
-- Send a message to chat
data ChatMessage = ChatMessage
  { chatRoomId :: Text
  , text       :: Text
  }
  deriving (Show, Generic)

instance Command ChatMessage where
  toArgs cmd = Json.Array
    [ Json.String "chat_message"
    , Json.String (chatRoomId cmd)
    , Json.String (text cmd)
    ]
