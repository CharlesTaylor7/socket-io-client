module Generals.Map.Types.Optics where

import Generals.Map.Types.Definitions


_Army :: Traversal' Tile Army
_Army = _Clear `failing` _City `failing` _General

_Owner :: Traversal' Tile Owner
_Owner = _Army . owner
