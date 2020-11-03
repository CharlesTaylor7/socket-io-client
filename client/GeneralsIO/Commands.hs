{-# Language DuplicateRecordFields #-}
{-# Language OverloadedLists #-}
{-# Language NamedFieldPuns #-}
{-# Language RecordWildCards #-}
{-# Language DeriveGeneric #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module GeneralsIO.Commands where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Aeson
import qualified Data.Aeson.Types as Json


class Show cmd => Command cmd where
  toArgs :: cmd -> Json.Array


-- | set_username
-- Register username to bot
data SetUsername = SetUsername
  { userId   :: Text
  , username :: Text
  }
  deriving (Show, Generic)

instance Command SetUsername where
  toArgs (SetUsername userId username) =
    [ toJSON "set_username"
    , toJSON userId
    , toJSON username
    ]

-- | play
-- Join FFA queue
data Play = Play
  { userId   :: Text
  }
  deriving (Show, Generic)

instance Command Play where
  toArgs Play{..} =
    [ toJSON "play"
    , toJSON userId
    ]

-- | join_1v1
-- Join 1v1 queue
data Join1v1 = Join1v1
  { userId   :: Text
  }
  deriving (Show, Generic)

instance Command Join1v1 where
  toArgs Join1v1{..} =
    [ toJSON "join_1v1"
    , toJSON userId
    ]

-- | join_private
-- Join a private game
data JoinPrivate = JoinPrivate
  { customGameId :: Text
  , userId       :: Text
  }
  deriving (Show, Generic)

instance Command JoinPrivate where
  toArgs JoinPrivate{..} =
    [ toJSON "join_private"
    , toJSON customGameId
    , toJSON userId
    ]

-- | set_custom_team
-- Choose team number in a custom game
data SetCustomTeam = SetCustomTeam
  { customGameId :: Text
  , team         :: Int -- ^ between 1 & 8 inclusive
  }
  deriving (Show, Generic)

instance Command SetCustomTeam where
  toArgs SetCustomTeam{..} =
    [ toJSON "set_custom_team"
    , toJSON customGameId
    , toJSON team
    ]

-- | join_team
-- join a team in the 2v2 queue
data JoinTeam = JoinTeam
  { teamId :: Text
  , userId :: Int
  }
  deriving (Show, Generic)

instance Command JoinTeam where
  toArgs JoinTeam{..} =
    [ toJSON "join_team"
    , toJSON teamId
    , toJSON userId
    ]

-- | leave_team
-- leave a team in the 2v2 queue
data LeaveTeam = LeaveTeam
  { teamId :: Text
  }
  deriving (Show, Generic)

instance Command LeaveTeam where
  toArgs LeaveTeam{..} =
    [ toJSON "leave_team"
    , toJSON teamId
    ]

-- | cancel
-- leave the current game queue
data Cancel = Cancel
  deriving (Show, Generic)

instance Command Cancel where
  toArgs Cancel = [ toJSON "cancel" ]

-- | set_force_start
-- Toggle force start status in multiplayer game
data SetForceStart = SetForceStart
  { queueId :: Text
  , doForce :: Bool
  }
  deriving (Show, Generic)

instance Command SetForceStart where
  toArgs SetForceStart{..}  =
    [ toJSON "set_force_start"
    , toJSON queueId
    , toJSON doForce
    ]

-- | attack
-- Make a move in game
data Attack = Attack
  { start    :: Int
  , end      :: Int
  , onlyHalf :: Bool
  }
  deriving (Show, Generic)

instance Command Attack where
  toArgs Attack{..}  =
    [ toJSON "attack"
    , toJSON start
    , toJSON end
    , toJSON onlyHalf
    ]

-- | clear_moves
-- Clear move queue
data ClearMoves = ClearMoves
  deriving (Show, Generic)

instance Command ClearMoves where
  toArgs ClearMoves = [ toJSON "clear_moves" ]

-- | ping_tile
-- Signals a tile to all teammates in a team game
data PingTile = PingTile
  { tile  :: Int
  }
  deriving (Show, Generic)

instance Command PingTile where
  toArgs PingTile{..} =
    [ toJSON "ping_tile"
    , toJSON tile
    ]

-- | chat_message
-- Send a message to chat
data ChatMessage = ChatMessage
  { chatRoomId :: Text
  , text       :: Text
  }
  deriving (Show, Generic)

instance Command ChatMessage where
  toArgs ChatMessage{..} =
    [ toJSON "chat_message"
    , toJSON chatRoomId
    , toJSON text
    ]

-- | leave_game
-- Leave game
data LeaveGame = LeaveGame
  deriving (Show, Generic)

instance Command LeaveGame where
  toArgs LeaveGame = [ toJSON "leave_game" ]

-- | stars_and_rank
-- Ask for a users stars & rank
data StarsAndRank = StarsAndRank
  { userId :: Text
  }
  deriving (Show, Generic)

instance Command StarsAndRank where
  toArgs StarsAndRank{..} =
    [ toJSON "stars_and_rank"
    , toJSON userId
    ]
