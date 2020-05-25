{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
module Generals.Map.Types.Definitions where

import Prelude hiding (Map)
import qualified Prelude as Containers

import Reflex
import Data.Default

import Types

data Tile
  = Clear Army
  | City Army
  | General Army
  | Mountain
  | Fog_Clear
  | Fog_Obstacle
  deriving (Show)

instance Default Tile where
  def = Clear def

data Owner
  = Neutral
  | Player Int
  deriving (Show)

instance Default Owner where
  def = Neutral

data Army = Army
  { _owner :: Owner
  , _size :: Int
  }
  deriving (Show)

instance Default Army where
  def = Army
    { _owner = def
    , _size = 0
    }

newtype GridIndex = GridIndex Int
  deriving (Eq, Ord)

type Grid = IntMap Tile

data Map t = Map
  { _tiles :: Dynamic t Grid
  , _dimensions :: Dimensions
  }


makePrisms ''Owner
makePrisms ''Tile
makePrisms ''GridIndex

makeFieldsNoPrefix ''Army
makeFieldsNoPrefix ''Map
