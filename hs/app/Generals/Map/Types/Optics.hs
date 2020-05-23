module Generals.Map.Types.Optics where

import Generals.Map.Types.Definitions


_Army :: Traversal' Tile Army
_Army = _Clear `failing` _City `failing` _General

_Owner :: Traversal' Tile Owner
_Owner = _Army . owner

-- iso from linear coordinates to grid coordinates
-- sue me its a bad pun
coordinated :: Int -> Iso' Int (Int, Int)
coordinated rowLength = iso to from
  where
    to index =
      let (j, i) = index `divMod` rowLength
      in (i+1, j+1)
    from (i, j) =
      (j - 1) * rowLength + (i - 1)
