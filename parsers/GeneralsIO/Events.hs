{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
module GeneralsIO.Events
{--
  ( SetUsernameError(..)
  , SetUsernameResponse(..)
  )
--}
  where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Vector (Vector)

import Data.Aeson (FromJson(..))
import qualified Data.Aeson as Json


-- | pre_game_start
type PreGameStart = Json.Object

-- | notify
type Notify = Json.Object

-- | chat_message
type ChatMessage = Json.Object

-- | error_user_id
type ErrorUserId = Json.Object

-- | error_banned
type ErrorBanned = Json.Object

-- | game_start
type GameStart = Json.Object

-- | game_won
type GameWon = Json.Object

-- | game_lost
type GameLost = Json.Object

-- | game_over
type GameOver = Json.Object


-- | queue_update
data QueueUpdate = QueueUpdate
  { isForcing     :: Bool
  , usernames     :: Json.Array
  , map_title     :: Maybe Text
  , teams         :: Vector Int
  , lobbyIndex    :: Int
  , playerIndices :: Vector Int
  , numForce      :: Int
  , numPlayers    :: Int
  }
  deriving (Generic)


-- | error_set_username
-- Note: this event is a misnomer. the generals protocol sends this event with an empty string when the username is valid
--
data ErrorSetUsername
  = UsernameValid
  | UsernameTaken
  | UsernameTooLong
  | UsernameMustBeginWithBot
  | UsernameCannotBeChanged
  | UsernameProfanity
  deriving (Generic)


-- | errors messages mapped to union
-- empty strings means the username was successfully chosen
setUserNameResponses :: [(String, SetUsernameResponse)]
setUserNameResponses =
  [ ( ""
    , UsernameValid
    )
  , ( "You already have a username! Only Supporters can change usernames."
    , UsernameCannotBeChanged
    )
  , ( "Usernames of Bots must begin with [Bot]"
    , UsernameMustBeginWithBot
    )
  , ( "Username too long."
    , UsernameTooLong
    )
  , ( "This username is already taken."
    , UsernameTaken
    )
  , ( "This username contains profanity."
    , UsernameProfanity
    )
  ]
