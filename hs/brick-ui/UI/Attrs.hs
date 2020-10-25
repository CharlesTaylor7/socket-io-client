module UI.Attrs where

import Prelude hiding (Empty, on)
import Types

import Brick
import qualified Graphics.Vty as V

rgbColor :: Word8 -> Word8 -> Word8 -> V.Color
rgbColor = V.rgbColor @Word8

gridAttrMap :: AttrMap
gridAttrMap = attrMap V.defAttr $
  players <>
    [ ("obstacle", fg grey)
    , ("general", V.currentAttr `V.withStyle` V.standout)
    , ("city", V.currentAttr `V.withStyle` V.underline)
    ]
  where
    grey = rgbColor 0x71 0x6f 0x6f
    players =
      zipWith
        (\attr i -> (attrName $ "player" <> show i, attr))
        playerAttributes
        ([1..] :: [Int])

playerAttributes :: [V.Attr]
playerAttributes =
  [ fg V.brightBlue
  , fg V.red
  , fg V.green
  , fg purple
  , fg teal
  , fg V.magenta
  , fg V.cyan
  , fg orange
  ]
  where
    teal = rgbColor 0 0x80 0x80
    purple = rgbColor 0x80 0 0x80
    orange = rgbColor 0xea 0x45 0x11


terrainAttr :: Tile -> AttrName
terrainAttr (Clear _) = "clear"
terrainAttr (City _) = "city"
terrainAttr (General _) = "general"
terrainAttr (Swamp _) = "swamp"
terrainAttr Mountain  = "mountain"
terrainAttr Fog_Clear  = "fog"
terrainAttr Fog_Obstacle  = "fog"
