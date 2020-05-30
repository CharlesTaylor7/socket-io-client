module Component.TableGrid
  ( grid
  ) where

import Reflex hiding (elDynClass)
import Types

import Data.CSS.Types
import Data.Dom (elStyle, elStyle', elDynStyle, elDynStyle', elDynClass)
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


grid
  :: (DomBuilder t m, PostBuild t m)
  => Generals.Map t
  -> m ()
grid map = do
  let mapHeight = map ^. map_dimensions . dimensions_height
  let mapWidth  = map ^. map_dimensions . dimensions_width
  let
    gridIx :: (Int, Int) -> Traversal' Grid Tile
    gridIx = ix . linearize
      where linearize (i, j) = (j - 1) * mapWidth + (i - 1)

  elClass "table" "grid" $
    elClass "tbody" "" $
      for_ [1..mapHeight] $ \j ->
      elClass "tr" "" $
        for_ [1..mapWidth] $ \i ->

        let
          tileTraversal :: Traversal' Grid Tile
          tileTraversal = gridIx (i, j)

          nonzero :: (Eq a, Num a) => Prism' a a
          nonzero = from (non 0) . _Just

          dynClass = map ^. map_tiles
            <&> (preview tileTraversal >>> toClass)
          dynArmySize = map ^. map_tiles
            <&>
              (   preview (tileTraversal . _Army . army_size . nonzero . to show)
              >>> maybe "" identity
              )

        in
          elDynClass (Dom.Node "td") dynClass $ dynText dynArmySize

  blank

toClass :: Maybe Tile -> CSSClass
toClass tile =
  case tile of
    Just (Clear _) ->    Class "clear"
    Just (City _) ->     Class "city"
    Just (General _) ->  Class "general"
    Just (Swamp _) ->    Class "swamp"
    Just Mountain ->     Class "mountain"
    Just Fog_Clear ->    Class "fog-clear"
    Just Fog_Obstacle -> Class "fog-obstacle"
    _ ->                 Class "unknown"
