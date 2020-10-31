module UI.Core.Attrs where

import Prelude hiding (Empty, on)
import Generals.Types

import Brick
import qualified Graphics.Vty as V

-- colors
rgbColor :: Word8 -> Word8 -> Word8 -> V.Color
rgbColor = V.rgbColor @Word8

grey    = rgbColor 0x71 0x6f 0x6f
blue    = V.brightBlue
red     = V.red
cyan    = V.cyan
magenta = V.magenta
green   = rgbColor 0x2f 0xd2 0x49
purple  = rgbColor 0x80 0    0x80
teal    = rgbColor 0    0x80 0x80
orange  = rgbColor 0xff 0x6c 0x02


combinations :: Semigroup a => [a] -> [a] -> [a]
combinations outer inner = do
  outerItem <- outer
  innerItem <- inner
  pure $ outerItem <> innerItem

playerStatusAttributes :: [(AttrName, V.Attr)]
playerStatusAttributes =
  [ ("alive", mempty)
  , ("dead", bg V.black)
  ]

playerAttributes :: [(AttrName, V.Attr)]
playerAttributes =
  zipWith
    (\color i -> (attrName $ "player" <> show i, fg color `V.withStyle` V.bold))
    colors
    ([1..] :: [Int])
  & (("neutral", fg V.black `V.withStyle` V.bold) :)
  where
    colors =
      [ red
      , blue
      , purple
      , magenta
      , cyan
      , green  -- needs to be darker
      , orange -- needs to be darker
      , teal   -- needs to be replaced
      ]
ownedTerrainAttributes :: [(AttrName, V.Attr)]
ownedTerrainAttributes =
  [ ("clear",    mempty)
  , ("city",     mempty `V.withStyle` V.underline)
  , ("general",  mempty `V.withStyle` V.standout)
  , ("swamp",    mempty `V.withStyle` V.strikethrough)
  ]


ownerAttr :: Tile -> AttrName
ownerAttr = view $
  (_Owner . #_Player . to (+1) . to show . to ("player" <>) . to attrName)
  `failing`
  like "neutral"

terrainAttr (Clear _) = "clear"
terrainAttr (City _) = "city"
terrainAttr (General _) = "general"
terrainAttr (Swamp _) = "swamp"
terrainAttr Mountain  = "mountain"
terrainAttr Fog_Clear  = "fog"
terrainAttr Fog_Obstacle  = "fog"
