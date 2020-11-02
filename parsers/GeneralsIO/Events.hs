{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# Language OverloadedLists #-}
{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
module GeneralsIO.Events where

import GHC.Generics (Generic)

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


-- | Generals Event
data Event
  = Connect
  | Disconnect
  | QueueUpdate QueueUpdate
  | ChatMessage ChatMessage
  | Notify Notify
  | PreGameStart PreGameStart
  | GameStart GameStart
  | GameWon GameWon
  | GameLost GameLost
  | GameOver GameOver
  | Rank Rank
  | Stars Stars
  | ErrorUserId ErrorUserId
  | ErrorBanned ErrorBanned
  | ErrorSetUsername ErrorSetUsername
  deriving (Generic, Show)

instance FromJSON Event where
  parseJSON = Json.withArray "Event" $ \v -> do
    eventName <- v .@ 0
    obj <- v .@ 1 <|> pure Json.Null

    withContext (Json.Key eventName) $
      case eventName of
        "connect"      -> pure Connect
        "disconnect"   -> pure Disconnect
        "queue_update" -> QueueUpdate <$> parseJSON obj
        "chat_message" -> ChatMessage <$> parseJSON (Json.Array v)
        "error_set_username" -> ErrorSetUsername <$> parseJSON obj

        _ -> fail $ "invalid event of: " <> T.unpack eventName

withContext :: Json.JSONPathElement -> Json.Parser a -> Json.Parser a
withContext = flip (<?>)

-- helpers
explicitParseAt :: (Json.Value -> Json.Parser a) -> Json.Array -> Int -> Json.Parser a
explicitParseAt p array key =
  case array !? key of
    Nothing -> fail $ "key " ++ show key ++ " not found"
    Just v  -> p v <?> Json.Index key

(.@) :: FromJSON a => Json.Array -> Int -> Json.Parser a
(.@) = explicitParseAt parseJSON

-- | rank
type Rank = Json.Value

-- | stars
type Stars = Json.Value

-- | pre_game_start
type PreGameStart = Json.Value

-- | notify
type Notify = Json.Value

-- | chat_message
data ChatMessage = MkChatMessage
  { chatRoomId :: Text
  , text :: Text
  }
  deriving (Show)

instance FromJSON ChatMessage where
  parseJSON = withArray "ChatMessage" $ \v -> do
    chatRoomId <- v .@ 1
    inner <- v .@ 2
    text <- inner & withObject "inner" (.: "text")
    pure $ MkChatMessage { chatRoomId, text }


-- | error_user_id
type ErrorUserId = Json.Value

-- | error_banned
type ErrorBanned = Json.Value

-- | game_start
type GameStart = Json.Value

-- | game_won
type GameWon = Json.Value

-- | game_lost
type GameLost = Json.Value

-- | game_over
type GameOver = Json.Value


-- | queue_update
data QueueUpdate = MkQueueUpdate
  { isForcing     :: Bool
  , usernames     :: Vector (Maybe Text)
  , map_title     :: Maybe Text
  , teams         :: Vector Int
  , lobbyIndex    :: Int
  , playerIndices :: Vector Int
  , numForce      :: Int
  , numPlayers    :: Int
  }
  deriving (Generic, Show)

instance FromJSON QueueUpdate


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
  deriving (Generic, Show)

instance FromJSON ErrorSetUsername where
  parseJSON = withText "error_set_username" $ \v -> do
      case usernameErrors HashMap.!? v of
        Just e -> pure e
        Nothing -> fail $ T.unpack v


usernameErrors :: HashMap Text ErrorSetUsername
usernameErrors =
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
