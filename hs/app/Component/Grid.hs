{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Grid where

import Reflex
import Types

import Data.CSS
import Data.Dom (elStyle, elStyle', elDynStyle, elDynStyle')
import qualified Data.Dom as Dom


grid :: (DomBuilder t m) => Dimensions -> m ()
grid dimensions =
  elStyle Dom.div gridStyle $
    for_ [1..(dimensions ^. height)] $ \j ->
    elStyle Dom.div rowStyle $
      for_ [1..(dimensions ^. width)] $ \i ->
      elStyle Dom.div tileStyle $ pure ()
  where
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
