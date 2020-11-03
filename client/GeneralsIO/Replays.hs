{-# Language RecordWildCards #-}
{-# Language DeriveGeneric #-}
module GeneralsIO.Replays where

import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Aeson.Types

import qualified Data.Aeson as Json

-- steal helper from events parser
import GeneralsIO.Events ((.@))


type GridIndex = Int

data Replay = Replay
  { version    :: Int
  , id         :: Text
  , mapWidth   :: Int
  , mapHeight  :: Int
  , usernames  :: Vector Text
  , stars      :: Json.Value
  , cities     :: Vector GridIndex
  , cityArmies :: Vector GridIndex
  , generals   :: Vector GridIndex
  , mountains  :: Vector GridIndex
  , moves      :: [Move]
  , afks       :: Json.Array
  , teams      :: Maybe Json.Array
  , mapTitle   :: Maybe Text
  , swamps     :: Vector GridIndex
  , unknown14  :: Json.Array
  , unknown15  :: Json.Array
  }
  deriving (Show, Generic)


instance FromJSON Replay where
  parseJSON = withArray "Replay" parseReplay

parseReplay :: Array -> Parser Replay
parseReplay v = do
  version    <- v .@ 0
  id         <- v .@ 1
  mapWidth   <- v .@ 2
  mapHeight  <- v .@ 3
  usernames  <- v .@ 4
  stars      <- v .@ 5
  cities     <- v .@ 6
  cityArmies <- v .@ 7
  generals   <- v .@ 8
  mountains  <- v .@ 9
  moves      <- v .@ 10
  afks       <- v .@ 11
  teams      <- v .@ 12
  mapTitle   <- v .@ 13
  unknown14  <- v .@ 14
  unknown15  <- v .@ 15
  swamps     <- v .@ 16
  pure Replay {..}


data Move = Move
  { playerIndex :: Int
  , startTile :: GridIndex
  , endTile :: GridIndex
  , onlyHalf :: Bool
  , turn :: Int
  }
  deriving (Show, Generic)

instance FromJSON Move where
  parseJSON = withArray "Move" $ \v -> do
    playerIndex <- v .@ 0
    startTile   <- v .@ 1
    endTile     <- v .@ 2
    onlyHalf    <- v .@ 3 <&> truthy
    turn        <- v .@ 4
    pure Move {..}
    where
      truthy :: Int -> Bool
      truthy = (/= 0)
