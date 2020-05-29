{-# language TemplateHaskell #-}
module Generals.Map.Types.Definitions where

import Prelude hiding (Map)
import qualified Prelude as Containers

import Data.Aeson (FromJSON(..))
import Reflex
import Data.Default

import Types

data Tile
  = Clear Army
  | City Army
  | General Army
  | Swamp Army
  | Mountain
  | Fog_Clear
  | Fog_Obstacle
  deriving (Show)

data ArmyTileType
  = Clear_Tile
  | City_Tile
  | General_Tile
  | Swamp_Tile
  deriving (Eq)

instance Default Tile where
  def = Clear def

data Owner
  = Neutral
  | Player Int
  deriving (Show, Eq)

instance Default Owner where
  def = Neutral

data Army = Army
  { _army_owner :: Owner
  , _army_size :: Int
  }
  deriving (Show, Eq)

instance Default Army where
  def = Army
    { _army_owner = def
    , _army_size = coerce (def :: Sum Int)
    }

newtype GridIndex = GridIndex Int
  deriving newtype (Eq, Ord, FromJSON)
  deriving stock (Show)

type Grid = IntMap Tile

data Map t = Map
  { _map_turn :: Dynamic t Int
  , _map_tiles :: Dynamic t Grid
  , _map_dimensions :: Dimensions
  }


makePrisms ''Owner
makePrisms ''Tile
makePrisms ''ArmyTileType
makePrisms ''GridIndex

makeLenses ''Army
makeLenses ''Map
