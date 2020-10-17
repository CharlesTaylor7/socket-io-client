module Types where

import Control.Lens.Unsafe (singular, (^?!))

import Data.Aeson (Array(..), FromJSON(..))
import Data.IntSet (IntSet)
import Data.Vector (Vector)


newtype GameConfig = GameConfig [BotName]
  deriving (Show)

newtype BotName = BotName Text
  deriving (Show)

newtype Id = Id Int
  deriving (Show)


data Tile
  = Clear Army
  | City Army
  | General Army
  | Swamp Army
  | Mountain
  | Fog_Clear
  | Fog_Obstacle
  deriving (Show, Generic)

data ArmyTileType
  = Clear_Tile
  | City_Tile
  | General_Tile
  | Swamp_Tile
  deriving (Eq, Generic)

instance Default Tile where
  def = Clear def

data Owner
  = Neutral
  | Player Int
  deriving (Show, Eq, Generic)

instance Default Owner where
  def = Neutral

data Army = Army
  { owner :: Owner
  , size :: Int
  }
  deriving (Show, Eq, Generic)

instance Default Army where
  def = Army
    { owner = def
    , size = coerce (def :: Sum Int)
    }


ixGrid :: GridIndex -> Lens' Grid Tile
ixGrid (GridIndex i) =
  singular
  ("Grid index: " <> show i)
  (#_Grid . ix i)


match :: Traversal' s a -> Traversal' s s
match matcher = filtered $ is _Just . firstOf matcher

_Army :: Traversal' Tile Army
_Army = #_Clear `failing` #_City `failing` #_General `failing` #_Swamp

_Owner :: Traversal' Tile Owner
_Owner = _Army . #owner

armyTileType :: Lens' Tile ArmyTileType
armyTileType = lens getter setter
  where
    getter = \case
      Clear _ -> Clear_Tile
      City _ -> City_Tile
      General _ -> General_Tile
      Swamp _ -> Swamp_Tile
    setter s b = s ^?! _Army . to construct $ "armyTileType lens"
      where
        construct =
          case b of
            Clear_Tile -> Clear
            City_Tile -> City
            General_Tile -> General
            Swamp_Tile -> Swamp

newtype GridIndex = GridIndex Int
  deriving newtype (Eq, Ord, FromJSON)
  deriving stock (Show, Generic)

instance Wrapped GridIndex

_GridIndex :: Iso' GridIndex Int
_GridIndex = _Wrapped'

newtype Grid = Grid (IntMap Tile)
  deriving stock (Show, Generic)

instance Wrapped Grid

_Grid :: Iso' Grid (IntMap Tile)
_Grid = _Wrapped'

type Turn = (Int, [Move])
type Turns = [Turn]

type History = Vector GameInfo

data Kill = Kill
  { killer :: Int
  , mark   :: Int
  }
  deriving (Show, Eq, Generic)

data GameInfo = GameInfo
  { grid         :: !Grid
  , activeCities :: !IntSet
  , activeSwamps :: !IntSet
  , owned        :: !(IntMap IntSet)
  -- kills, in reverse chronological order
  , kills        :: ![Kill]
  -- get only
  , numTiles     :: !Int
  , replay       :: !Replay
  }
  deriving (Generic)
-- replays

data Server
  = Server_Main
  | Server_Bot
  deriving (Eq, Ord, Generic)

data ReplayLocation = ReplayLocation
  { server :: Server
  , id :: Text
  }
  deriving (Eq, Ord, Generic)

type GridLocations = Vector Int

data Replay = Replay
  { id :: Text
  , mapWidth :: Int
  , mapHeight :: Int
  , usernames :: Vector Text

  , cities :: GridLocations
  , cityArmies :: GridLocations
  , generals :: GridLocations
  , mountains :: GridLocations
  , moves :: [Move]

  , afks :: Array
  , teams :: Maybe Array
  , mapTitle :: Maybe Text
  , swamps :: GridLocations
  -- , unknown1 :: Array
  -- , unknown2 :: Array
  }
  deriving (Show, Generic)

data Move = Move
  { playerIndex :: Int
  , startTile :: GridIndex
  , endTile :: GridIndex
  , onlyHalf :: Bool
  , turn :: Int
  }
  deriving (Eq, Show, Generic)

data Command
  = DoNothing
  | Backwards
  | Forwards
  | JumpTo TurnIndex
  deriving (Show, Generic)


instance Semigroup Command where
  j@(JumpTo _) <> _     = j
  _ <> j@(JumpTo _)     = j
  anything <> DoNothing = anything
  _ <> latest           = latest


instance Monoid Command where
  mempty = DoNothing

instance Default Command where
  def = DoNothing

newtype TurnIndex = TurnIndex Int
  deriving stock (Show)
  deriving newtype (Ord, Eq, Num)
  deriving (Default, Semigroup, Monoid) via Sum Int


data Perspective
  = Global
  | Perspective Int
  deriving (Show, Eq)

