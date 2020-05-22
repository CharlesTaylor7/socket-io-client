{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Grid where

import Reflex
import Types

import Data.CSS.Types
import Data.Dom (elStyle, elStyle', elDynStyle, elDynStyle')
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

_ShowText :: (Show a, Read a) => Prism' Text a
_ShowText = unpacked . _Show

img :: DomBuilder t m => Text -> m ()
img name = elAttr "img" ("href" =: url) blank
  where url = "/img/" <> name


grid :: (DomBuilder t m) => Generals.Map -> m ()
grid Generals.Map {..} =
  elStyle Dom.div gridStyle $
    for_ [1..(dimensions ^. height)] $ \j ->
    elStyle Dom.div rowStyle $
      for_ [1..(dimensions ^. width)] $ \i ->
      tileElement i j
  where
    tileElement i j =
      let tile = tiles ^? ix (i, j)
      in elStyle Dom.div (tileStyle tile) $
      case tile of
        Just (Clear army) ->   elClass "span" "clear"    $ toTextNode army
        Just (City army) ->    elClass "span" "city"     $ toTextNode army
        Just (General army) -> elClass "span" "general"  $ toTextNode army
        Just Mountain ->       elClass "span" "mountain" $ blank
        _ ->                   elClass "span" "unknown"  $ text "???"
    generalStyle army = def
      & cssClass . _Class .~ "general"

    toTextNode army = text $
      case army ^. size . from (non 0) of
        Just n -> n ^. re _ShowText
        Nothing -> ""
    gridStyle = def
      & cssClass . _Class .~ "grid"
      & inlineStyle . at "width" ?~ toText gridWidth
    rowStyle = def
      & cssClass . _Class .~ "row"
    tileStyle tile = def
      & cssClass .~ (Class "tile" <> toCssClass tile)
      & inlineStyle %~
        (at "width" ?~ sideLength) .
        (at "height" ?~ sideLength)
      where
        army = tile ^. singular (_Just . _Army `failing` like (Neutral `Army` 0))

    sideLength = toText tileSideLength

    tileSideLength :: Pixels
    tileSideLength = 40

    gridWidth :: Pixels
    gridWidth = tileSideLength * (dimensions ^. width . to fromIntegral)

toCssClass :: Maybe Tile -> CSSClass
toCssClass tile = ownerClass <> emptyClass <> terrainClass
  where
    ownerClass = Class $
      case tile ^? _Just . _Owner of
        Just (Player id) -> "tile-owner-" <> show id
        Just Neutral     -> "tile-neutral"
        _                -> ""

    emptyClass = Class $
      case tile ^? _Just . _Army . size of
        Just 0 -> "tile-empty"
        _      -> ""

    terrainClass = Class $
      case tile of
        Just Mountain -> "tile-mountain"
        _             -> ""
