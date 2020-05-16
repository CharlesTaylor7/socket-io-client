{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Page.Game.Grid where

import Generals.Imports
import Generals.Types

import Data.CSS
import Data.Dom (elStyle, elStyle', elDynStyle, elDynStyle')
import qualified Data.Dom as Dom

tileSideLength :: Pixels
tileSideLength = 40

gridWidth :: Pixels
gridWidth = tileSideLength * (dimensions ^. width . to fromIntegral)

dimensions :: Dimensions
dimensions = Dimensions { _width = 7, _height = 5}

grid :: (DomBuilder t m) => m ()
grid =
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
