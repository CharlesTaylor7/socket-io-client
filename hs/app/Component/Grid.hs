module Component.Grid
  ( gridDynStyle
  ) where

import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


gridDynStyle
  :: (DomBuilder t m, PostBuild t m)
  => (Int, Int)
  -> Dynamic t Grid
  -> Dynamic t Style
  -> m ()
gridDynStyle (mapWidth, mapHeight) gridDyn gridStyle =
  elDynStyle "table" (gridStyle <&> style_cssClass .~ Class "grid") $
    elClass "tbody" "" $
      for_ [1..mapHeight] $ \j ->
      elClass "tr" "" $
        for_ [1..mapWidth] $ \i ->
        tileElement (Width mapWidth) gridDyn (i, j)

newtype Width = Width Int


tileElement
  :: forall t m. (DomBuilder t m, PostBuild t m)
  => Width
  -> Dynamic t Grid
  -> (Int, Int)
  -> m ()
tileElement (Width mapWidth) gridDyn coords =
  let
    gridIx :: (Int, Int) -> Traversal' Grid Tile
    gridIx (i, j) = _Grid . ix index
      where
        index = (j - 1) * mapWidth + (i - 1)

    tileTraversal :: Traversal' Grid Tile
    tileTraversal = gridIx coords

    nonzero :: (Eq a, Num a) => Prism' a a
    nonzero = from (non 0) . _Just

    dynClass :: Dynamic t CSSClass
    dynClass = gridDyn
      <&> (preview tileTraversal >>> maybe mempty toClass)

    dynArmyText :: Dynamic t Text
    dynArmyText = gridDyn
      <&>
        (   preview (tileTraversal . _Army . army_size . nonzero . to show)
        >>> maybe "" identity
        )
  in
    elDynClass (Dom.Node "td") dynClass $
        dynText dynArmyText


toClass :: Tile -> CSSClass
toClass tile =
  let
    terrain =
      case tile of
        Clear _ ->      Class "clear"
        City _ ->       Class "city"
        General _ ->    Class "general"
        Swamp _ ->      Class "swamp"
        Mountain ->     Class "mountain"
        Fog_Clear ->    Class "fog clear"
        Fog_Obstacle -> Class "fog obstacle"
    owner =
      case tile ^? _Owner of
        Just (Player id) -> Class $ "player-" <> show id
        Just Neutral -> Class "neutral"
        Nothing -> mempty
  in
    terrain <> owner
