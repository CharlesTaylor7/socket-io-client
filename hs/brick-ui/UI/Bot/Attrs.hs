module UI.Bot.Attrs
  ( module Core
  , gridAttrMap
  )
  where
  )
  where

import Prelude hiding (Empty, on)
import Generals.Types

import Brick
import qualified Graphics.Vty as V

import UI.Core.Attrs as Core


gridAttrMap :: AttrMap
gridAttrMap = attrMap V.defAttr
  $  combinations playerAttributes ownedTerrainAttributes
  <> combinations playerAttributes playerStatusAttributes
  <> playerAttributes
  where
    grey = rgbColor 0x71 0x6f 0x6f
