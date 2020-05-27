module Generals.Map.Types.Optics where

import Generals.Map.Types.Definitions

ixGrid :: GridIndex -> Lens' Grid Tile
ixGrid = singular . ix . coerce

match :: Traversal' s a -> Traversal' s s
match matcher = filtered $ is _Just . firstOf matcher


_Army :: Traversal' Tile Army
_Army = _Clear `failing` _City `failing` _General `failing` _Swamp

_Owner :: Traversal' Tile Owner
_Owner = _Army . owner

armyTileType :: Lens' Tile ArmyTileType
armyTileType = lens getter setter
  where
    getter = \case
      Clear _ -> Clear_Tile
      City _ -> City_Tile
      General _ -> General_Tile
      Swamp _ -> General_Tile
    setter s b = s ^?! _Army . to construct
      where
        construct =
          case b of
            Clear_Tile -> Clear
            City_Tile -> City
            General_Tile -> General
            Swamp_Tile -> Swamp
