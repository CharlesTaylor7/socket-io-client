module Generals.Map.Types.Optics where

import Generals.Map.Types.Definitions


_Army :: Traversal' Tile Army
_Army = _Clear `failing` _City `failing` _General

_Owner :: Traversal' Tile Owner
_Owner = _Army . owner

tileType :: Lens' Tile ArmyTileType
tileType = lens getter setter
  where
    getter = \case
      Clear _ -> Clear_Tile
      City _ -> City_Tile
      General _ -> General_Tile
    setter s b = s ^?! _Army . to construct
      where
        construct =
          case b of
            Clear_Tile -> Clear
            City_Tile -> City
            General_Tile -> General
