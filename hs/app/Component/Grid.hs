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

grid :: (DomBuilder t m) => Generals.Map -> m ()
grid Generals.Map {..} =
  elStyle Dom.div gridStyle $
    for_ [1..(dimensions ^. height)] $ \j ->
    elStyle Dom.div rowStyle $
      for_ [1..(dimensions ^. width)] $ \i ->
      tile i j
  where
    tile i j = elStyle Dom.div tileStyle $
      case cells ^? ix (i, j) of
        Nothing -> text "??"
        Just (Clear army) -> text $ show $ size army
        Just (City army) -> text $ show $ size army
        Just Mountain -> text "M"


    gridStyle = def
      & cssClass . _Class .~ "grid"
      & inlineStyle . at "width" ?~ toText gridWidth
    rowStyle = def
      & cssClass . _Class .~ "row"
    tileStyle = def
      & cssClass . _Class .~ "tile"
      & inlineStyle %~
        (at "width" ?~ sideLength) .
        (at "height" ?~ sideLength)
    sideLength = toText tileSideLength

    tileSideLength :: Pixels
    tileSideLength = 40

    gridWidth :: Pixels
    gridWidth = tileSideLength * (dimensions ^. width . to fromIntegral)
