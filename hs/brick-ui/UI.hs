module UI where

import Prelude hiding (Empty, on)
import Types
import Control.Lens.Unsafe

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick as Scroll (ViewportType(..))

import Brick.Widgets.Border.Style
import Brick.Widgets.Center

import Brick.Grid (GridStyle(..))
import qualified Brick.Grid as Grid

import qualified Data.Text as T

import UI.Types
import UI.Attrs
import UI.Events


-- | show army count in 4 characters
-- if the army is over 4 digits, use k prefix
-- if the army is under 4 digits pad with space characters
showArmyCount :: Int -> Text
showArmyCount n
  | n < 10 = "  " <> show n <> " "
  | n < 100 = " " <> show n <> " "
  | n < 1_000 = show n <> " "
  | n < 10_000 = show n
  | n < 100_000 = " " <> show (n `div` 1_000) <> "k"
  | n < 1_000_000 = show (n `div` 1_000) <> "k"
  | otherwise = "lorj"

-- App definition
app :: App AppState Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const gridAttrMap
  }
drawUI :: AppState -> [Widget]
drawUI (history, TurnIndex turn) =
  [ center $ flip runReader gridStyle $ drawGrid game
  ]
  where
    game :: GameInfo
    game = history ^?! ix turn $ "history index"

    toTile :: (Int, Int) -> Tile
    toTile (i, j) = game ^?! #grid . #_Grid . ix (j * width + i) $ "grid index"

    width = game ^. #replay . #mapWidth
    height = game ^. #replay . #mapHeight

    gridStyle = GridStyle
      { borderStyle = unicode
      , cellWidth = 4
      , gridWidth = width
      , gridHeight = height
      , toTile = flip runReader gridStyle . drawTile . toTile
      }

-- | Draw tile
drawTile :: MonadReader GridStyle m => Tile -> m (Text, AttrName)
drawTile tile = do
  cellWidth <- view #cellWidth
  pure
    $ (tile ^. contents cellWidth, tile ^. ownerAttr <> tile ^. to terrainAttr)
  where
    ownerAttr :: Fold Tile AttrName
    ownerAttr =
      (_Owner . #_Player . to (+1) . to show . to ("player" <>) . to attrName)
      `failing`
      like "neutral"

    contents w =
      (_Army . #size . from (non 0) . _Just . to showArmyCount)
      `failing`
      like (T.replicate w " ")


drawTitle :: GameInfo -> Widget
drawTitle gameInfo = do
  let
    dimensions = show width <> "x" <> show height
    width = gameInfo ^. #replay . #mapWidth
    height = gameInfo ^. #replay . #mapHeight

  hCenter $ str $ "Replay " <> dimensions


drawGrid :: MonadReader GridStyle m => GameInfo -> m Widget
drawGrid gameInfo = do
  gridContent <- reader Grid.drawGrid
  let
    grid =
      viewport GridView Scroll.Both $
      cached GridView $
        gridContent

  pure $ drawTitle gameInfo <=> grid
