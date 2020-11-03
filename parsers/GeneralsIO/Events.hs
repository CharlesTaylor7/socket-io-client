{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# Language OverloadedLists #-}
{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
module GeneralsIO.Events where

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


-- | Generals Event
data Event
  = Connect Connect
  | Disconnect Disconnect
  | QueueUpdate QueueUpdate
  | ChatMessage ChatMessage
  | Notify Notify
 --  | PreGameStart PreGameStart
 --  | GameStart GameStart
 --  | GameWon GameWon
 --  | GameLost GameLost
 --  | GameOver GameOver
 --  | Rank Rank
 --  | Stars Stars
 --  | ErrorUserId ErrorUserId
 --  | ErrorBanned ErrorBanned
  | ErrorSetUsername ErrorSetUsername
  | Unknown Json.Value
  deriving (Generic, Show)

instance FromJSON Event where
  parseJSON v = (asum :: [Json.Parser Event] -> Json.Parser Event)
    [ Disconnect <$> parseJSON v
    , Connect    <$> parseJSON v
    , QueueUpdate <$> parseJSON v
    , ChatMessage <$> parseJSON v
    , Notify <$> parseJSON v
 --   , PreGameStart <$> parseJSON v
 --   , GameStart <$> parseJSON v
 --   , GameWon <$> parseJSON v
 --   , GameLost <$> parseJSON v
 --   , GameOver <$> parseJSON v
 --   , Rank <$> parseJSON v
 --   , Stars <$> parseJSON v
 --   , ErrorUserId <$> parseJSON v
 --   , ErrorBanned <$> parseJSON v
    , ErrorSetUsername <$> parseJSON v
    , pure $ Unknown v
    ]

data Connect = MkConnect
  deriving (Show)

data Disconnect = MkDisconnect
  deriving (Show)

instance FromJSON Connect where
  parseJSON = withEvent "connect" (const $ pure MkConnect)

instance FromJSON Disconnect where
  parseJSON = withEvent "disconnect" (const $ pure MkDisconnect)

-- | rank
type Rank = Json.Value

-- | stars
type Stars = Json.Value

-- | pre_game_start
type PreGameStart = Json.Value

-- | notify
data Notify = MkNotify
  { event :: Text
  , info :: Text
  }
  deriving (Show)

instance FromJSON Notify where
  parseJSON = withEvent "notify" $ \v -> do
    event <- v .@ 1
    info <- v .@ 2
    pure $ MkNotify { event, info }


-- | chat_message
data ChatMessage = MkChatMessage
  { chatRoomId :: Text
  , text :: Text
  }
  deriving (Show)

instance FromJSON ChatMessage where
  parseJSON = withEvent "chat_message" $ \v -> do
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
  , mapTitle      :: Maybe Text
  , teams         :: Vector Int
  , lobbyIndex    :: Int
  , playerIndices :: Vector Int
  , numForcing    :: Int
  , numPlayers    :: Int
  }
  deriving (Generic, Show)

instance FromJSON QueueUpdate where
  parseJSON = withEvent "queue_update" $ \v -> do
    obj <- v .@ 1
    flip (withObject "QueueUpdate") obj $ \v ->
      MkQueueUpdate
        <$> v .: "isForcing"
        <*> v .: "usernames"
        <*> v .: "map_title"
        <*> v .: "teams"
        <*> v .: "lobbyIndex"
        <*> v .: "playerIndices"
        <*> v .: "numForce"
        <*> v .: "numPlayers"

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
  parseJSON = withEvent "error_set_username" $ \v -> do
    message <- v .@ 1
    flip (withText "message") message $ \v ->
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


-- helpers
withEvent :: Text -> (Json.Array -> Json.Parser a) -> Json.Value -> Json.Parser a
withEvent eventName continuation = do
  Json.withArray "Event" $ \v -> do
    firstArrayElement <- v .@ 0
    if firstArrayElement == eventName
    then continuation v
    else fail $ "not " <> T.unpack eventName

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


