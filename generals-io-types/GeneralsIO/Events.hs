{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedLists #-}
{-# Language OverloadedStrings #-}
module GeneralsIO.Events where

import GHC.Generics (Generic)

import Data.Foldable (asum)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector, (!?))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Aeson
import qualified Data.Aeson.Types as Json


-- | Generals Event
data Event
  = QueueUpdate QueueUpdate
  | ChatMessage ChatMessage
  | Notify Notify
  | PreGameStart PreGameStart
  | GameStart GameStart
  | GameUpdate GameUpdate
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
  parseJSON v = (asum :: [Json.Parser Event] -> Json.Parser Event)
    [ QueueUpdate <$> parseJSON v
    , ChatMessage <$> parseJSON v
    , Notify <$> parseJSON v
    , PreGameStart <$> parseJSON v
    , GameStart <$> parseJSON v
    , GameUpdate <$> parseJSON v
    , GameWon <$> parseJSON v
    , GameLost <$> parseJSON v
    , GameOver <$> parseJSON v
    , Rank <$> parseJSON v
    , Stars <$> parseJSON v
    , ErrorUserId <$> parseJSON v
    , ErrorBanned <$> parseJSON v
    , ErrorSetUsername <$> parseJSON v
    ]

data Disconnect = MkDisconnect
  deriving (Show)

instance FromJSON Disconnect where
  parseJSON = withEvent "disconnect" (const $ pure MkDisconnect)

-- | rank
type Rank = Json.Value

-- | stars
type Stars = Json.Value

-- | notify
data Notify = MkNotify
  { event :: Text
  , info :: Text
  }
  deriving (Show, Generic)

instance FromJSON Notify where
  parseJSON = withEvent "notify" $ \v -> do
    event <- v .@ 1
    info <- v .@ 2
    pure $ MkNotify {..}


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
    pure $ MkChatMessage {..}

-- | pre_game_start
data PreGameStart = MkPreGameStart
  deriving (Show )

instance FromJSON PreGameStart where
  parseJSON = withEvent "pre_game_start" (const $ pure MkPreGameStart)

-- | game_start
data GameStart = MkGameStart
  { playerIndex :: Int
  , usernames   :: Vector Text
  , swamps      :: Json.Array
  , teams       :: Vector Int
  , chat_room   :: Text
  , game_type   :: Text
  , replay_id   :: Text
  }
  deriving (Generic, Show)

instance FromJSON GameStart where
  parseJSON = withEvent "game_start" $ \v -> do
    obj <- v .@ 1
    flip (withObject "GameStart") obj $ \v ->
      MkGameStart
        <$> v .: "playerIndex"
        <*> v .: "usernames"
        <*> v .: "swamps"
        <*> v .: "teams"
        <*> v .: "chat_room"
        <*> v .: "game_type"
        <*> v .: "replay_id"


-- | game_update
data GameUpdate = MkGameUpdate
  { turn        :: Int
  , generals    :: Vector Int
  , citiesDiff  :: Vector Int
  , attackIndex :: Int
  , scores      :: Vector Score
  , mapDiff     :: Vector Int
  }
  deriving (Generic, Show)

instance FromJSON GameUpdate where
  parseJSON = withEvent "game_update" $ \v -> do
    obj <- v .@ 1
    flip (withObject "GameUpdate") obj $ \v ->
      MkGameUpdate
        <$> v .: "turn"
        <*> v .: "generals"
        <*> v .: "cities_diff"
        <*> v .: "attackIndex"
        <*> v .: "scores"
        <*> v .: "map_diff"

data Score = MkScore
  { dead  :: Bool
  , tiles :: Int
  , total :: Int
  , i     :: Int
  }
  deriving (Generic, Show)

instance FromJSON Score

-- | game_won
data GameWon = MkGameWon
  { unknown1 :: Json.Value
  , unknown2 :: Json.Value
  }
  deriving (Generic, Show)

instance FromJSON GameWon where
  parseJSON = withEvent "game_won" $ \v -> do
    MkGameWon
      <$> v .@ 1
      <*> v .@ 2

-- | game_lost
data GameLost = MkGameLost
  { killer :: Int
  , unknown :: Json.Value
  }
  deriving (Generic, Show)

instance FromJSON GameLost where
  parseJSON = withEvent "game_lost" $ \v -> do
    MkGameLost
      <$> (v .@ 1 >>= withObject "GameLost" (.: "killer"))
      <*> v .@ 2

-- | game_over
data GameOver = MkGameOver
  { unknown1 :: Json.Value
  , unknown2 :: Json.Value
  }
  deriving (Generic, Show)

instance FromJSON GameOver where
  parseJSON = withEvent "game_over" $ \v -> do
    MkGameOver
      <$> v .@ 1
      <*> v .@ 2

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

-- | error_user_id
type ErrorUserId = Json.Value

-- | error_banned
type ErrorBanned = Json.Value

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


