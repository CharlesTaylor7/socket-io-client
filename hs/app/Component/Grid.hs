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


grid
  :: (DomBuilder t m, PostBuild t m)
  => Generals.Map t
  -> m (Element t m)
grid map = do
  postBuild <- getPostBuild
  (element, _) <- elStyle' Dom.div (gridStyle (map ^. dimensions)) $
    for_ [1..(map ^. dimensions . height)] $ \j ->
    elStyle Dom.div rowStyle $
      for_ [1..(map ^. dimensions . width)] $ \i ->
      tileElement (sequenceA (map ^? tiles . ix (i, j)))
  notReadyUntil postBuild
  pure element

-- static grid
gridStyle dimensions = def
  & cssClass . _Class .~ "grid"
  & inlineStyle . at "width" ?~ (toText . gridWidth) dimensions

gridWidth :: Dimensions -> Pixels
gridWidth dimensions = tileSideLength * (dimensions ^. width . to fromIntegral)

rowStyle = def
  & cssClass . _Class .~ "row"

sideLength :: Text
sideLength = toText tileSideLength

tileSideLength :: Pixels
tileSideLength = 40

-- dynamic tiles
tileElement
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t (Maybe Tile)
  -> m ()
tileElement tile = elDynStyle Dom.div (tileStyle <$> tile) $
  let
    contents = getContents <$> tile
    cssClass = fst <$> contents
    text = snd <$> contents
  in
    tileContents cssClass text

tileContents
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t CSSClass
  -> Dynamic t Text
  -> m ()
tileContents (fmap (view _Class) -> cssClass) text =
  elDynClass "span" cssClass $
    dynText text

getContents :: Maybe Tile -> (CSSClass, Text)
getContents tile =
  case tile of
    Just (Clear army) ->   (Class "clear", armyToText army)
    Just (City army) ->    (Class "city", armyToText army)
    Just (General army) -> (Class "general", armyToText army)
    Just Mountain ->       (Class "mountain", "")
    _ ->                   (Class "unknown", "???")

armyToText :: Army -> Text
armyToText army =
  case army ^. size . from (non 0) of
    Just n -> n ^. re _ShowText
    Nothing -> ""

tileStyle tile = def
  & cssClass .~ (Class "tile" <> toCssClass tile)
  & inlineStyle %~
    (at "width" ?~ sideLength) .
    (at "height" ?~ sideLength)
  where
    army = tile ^. singular (_Just . _Army `failing` like (Neutral `Army` 0))

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
