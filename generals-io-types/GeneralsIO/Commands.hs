{-# Language DuplicateRecordFields #-}
{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
{-# Language RecordWildCards #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
module GeneralsIO.Commands where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.ByteString (ByteString)

import Jsonifier (Json)
import qualified Jsonifier as Json


data SomeCommand where
  SomeCommand
    :: forall cmd. (Show cmd, Command cmd)
    => cmd
    -> SomeCommand

encode :: Command cmd => cmd -> ByteString
encode = Json.toByteString . Json.array . toArgs


class Command cmd where
  toArgs :: cmd -> [Json]


-- | set_username
-- Register username to bot
data SetUsername = SetUsername
  { userId   :: Text
  , username :: Text
  }
  deriving (Show, Generic)

instance Command SetUsername where
  toArgs (SetUsername userId username) =
    [ Json.textString "set_username"
    , Json.textString userId
    , Json.textString username
    ]

-- | play
-- Join FFA queue
data Play = Play
  { botId :: Text
  }
  deriving (Show, Generic)

instance Command Play where
  toArgs Play{..} =
    [ Json.textString "play"
    , Json.textString botId
    ]

-- | join_1v1
-- Join 1v1 queue
data Join1v1 = Join1v1
  { botId :: Text
  }
  deriving (Show, Generic)

instance Command Join1v1 where
  toArgs Join1v1{..} =
    [ Json.textString "join_1v1"
    , Json.textString botId
    ]

-- | join_private
-- Join a private game
data JoinPrivate = JoinPrivate
  { gameId :: Text
  , botId  :: Text
  }
  deriving (Show, Generic)

instance Command JoinPrivate where
  toArgs JoinPrivate{..} =
    [ Json.textString "join_private"
    , Json.textString gameId
    , Json.textString botId
    ]

-- | set_custom_team
-- Choose team number in a custom game
data SetCustomTeam = SetCustomTeam
  { gameId :: Text
  , team         :: Int -- ^ between 1 & 8 inclusive
  }
  deriving (Show, Generic)

instance Command SetCustomTeam where
  toArgs SetCustomTeam{..} =
    [ Json.textString "set_custom_team"
    , Json.textString gameId
    , Json.intNumber team
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
    [ Json.textString "join_team"
    , Json.textString teamId
    , Json.intNumber userId
    ]

-- | leave_team
-- leave a team in the 2v2 queue
data LeaveTeam = LeaveTeam
  { teamId :: Text
  }
  deriving (Show, Generic)

instance Command LeaveTeam where
  toArgs LeaveTeam{..} =
    [ Json.textString "leave_team"
    , Json.textString teamId
    ]

-- | cancel
-- leave the current game queue
data Cancel = Cancel
  deriving (Show, Generic)

instance Command Cancel where
  toArgs Cancel = [ Json.textString "cancel" ]

-- | set_force_start
-- Toggle force start status in multiplayer game
data SetForceStart = SetForceStart
  { queueId :: Text
  , force   :: Bool
  }
  deriving (Show, Generic)

instance Command SetForceStart where
  toArgs SetForceStart{..}  =
    [ Json.textString "set_force_start"
    , Json.textString queueId
    , Json.bool force
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
    [ Json.textString "attack"
    , Json.intNumber start
    , Json.intNumber end
    , Json.bool onlyHalf
    ]

-- | clear_moves
-- Clear move queue
data ClearMoves = ClearMoves
  deriving (Show, Generic)

instance Command ClearMoves where
  toArgs ClearMoves = [ Json.textString "clear_moves" ]

-- | ping_tile
-- Signals a tile to all teammates in a team game
data PingTile = PingTile
  { tile  :: Int
  }
  deriving (Show, Generic)

instance Command PingTile where
  toArgs PingTile{..} =
    [ Json.textString "ping_tile"
    , Json.intNumber tile
    ]

-- | chat_message
-- Send a message to chat
data Message = Message
  { chatRoomId :: Text
  , text       :: Text
  }
  deriving (Show, Generic)

instance Command Message where
  toArgs Message{..} =
    [ Json.textString "chat_message"
    , Json.textString chatRoomId
    , Json.textString text
    ]

-- | leave_game
-- Leave game
data LeaveGame = LeaveGame
  deriving (Show, Generic)

instance Command LeaveGame where
  toArgs LeaveGame = [ Json.textString "leave_game" ]

-- | stars_and_rank
-- Ask for a users stars & rank
data StarsAndRank = StarsAndRank
  { userId :: Text
  }
  deriving (Show, Generic)

instance Command StarsAndRank where
  toArgs StarsAndRank{..} =
    [ Json.textString "stars_and_rank"
    , Json.textString userId
    ]
