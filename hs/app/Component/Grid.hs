module Component.Grid
  ( grid
  ) where

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
  -> m ()
grid map = do
  let mapHeight = map ^. map_dimensions . dimensions_height
  let mapWidth  = map ^. map_dimensions . dimensions_width
  let gridIx = ix . linearize
        where linearize (i, j) = (j - 1) * mapWidth + (i - 1)

  -- postBuild <- getPostBuild
  (element, _) <- elStyle' Dom.div (gridStyle (map ^. map_dimensions)) $
    for_ [1..mapHeight] $ \j ->
    elStyle Dom.div rowStyle $
      for_ [1..mapWidth] $ \i ->
      tileElement (map ^. map_tiles <&> preview (gridIx (i, j)))
  -- notReadyUntil postBuild
  blank

-- static grid
gridStyle dimensions = def
  & style_cssClass . _Class .~ "grid"
  & style_inline . at "width" ?~ (toText . gridWidth) dimensions

gridWidth :: Dimensions -> Pixels
gridWidth dimensions = tileSideLength * (dimensions ^. dimensions_width . to fromIntegral)

rowStyle = def
  & style_cssClass . _Class .~ "row"

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
    style_cssClass = fst <$> contents
    text = snd <$> contents
  in
    tileContents style_cssClass text

tileContents
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t CSSClass
  -> Dynamic t Text
  -> m ()
tileContents (fmap (view _Class) -> style_cssClass) text =
  elDynClass "span" style_cssClass $
    dynText text

getContents :: Maybe Tile -> (CSSClass, Text)
getContents tile =
  case tile of
    Just (Clear army) ->   (Class "clear", armyToText army)
    Just (City army) ->    (Class "city", armyToText army)
    Just (General army) -> (Class "general", armyToText army)
    Just Mountain ->       (Class "mountain", "")
    Just (Swamp army) ->   (Class "swamp", armyToText army)
    _ ->                   (Class "unknown", "???")

armyToText :: Army -> Text
armyToText army =
  case army ^. army_size . from (non 0) of
    Just n -> n ^. re _ShowText
    Nothing -> ""

tileStyle tile = def
  & style_cssClass .~ (Class "tile" <> toCssClass tile)
  & style_inline %~
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
      case tile ^? _Just . _Army . army_size of
        Just 0 -> "tile-empty"
        _      -> ""

    terrainClass = Class $
      case tile of
        Just Mountain -> "tile-mountain"
        Just (Swamp _) -> "tile-swamp"
        Just (Clear _) -> "tile-clear"
        _             -> ""
