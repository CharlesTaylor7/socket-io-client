module UI where

import Prelude hiding (Empty, on)
import Types
import Control.Lens.Unsafe

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick as Scroll (ViewportType(..))

import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Forms

import Brick.Grid (GridStyle(..))
import qualified Brick.Grid as Grid

import qualified Data.Text as T

import UI.Types
import UI.Attrs
import UI.Events

-- App definition
app :: App AppState CustomEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const gridAttrMap
  }
drawUI :: AppState -> [Widget]
drawUI appState =
  [ center $ form <=> grid
  ]
  where
    form = appState ^. #jumpToTurn . to renderForm
    history = appState ^. #history
    turn = appState ^. #turnIndex . _TurnIndex

    grid = flip runReader gridStyle $ drawGrid game

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

drawHeader :: GameInfo -> Widget
drawHeader gameInfo = do
  let
    dimensions = show width <> "x" <> show height
    turn = gameInfo ^. #turnIndex . _TurnIndex
    width = gameInfo ^. #replay . #mapWidth
    height = gameInfo ^. #replay . #mapHeight

    titleWidget = txt $ "Replay " <> dimensions
    turnInputWidget = renderForm $ jumpToTurnForm (TurnIndex turn)
  hCenter $ titleWidget


drawGrid :: MonadReader GridStyle m => GameInfo -> m Widget
drawGrid gameInfo = do
  gridContent <- reader Grid.drawGrid

  pure $
    viewport GridView Scroll.Vertical $
    hCenter $
    cached GridView $
      gridContent

jumpToTurnForm :: TurnIndex -> Form TurnIndex e Name
jumpToTurnForm =
  setFormFocus JumpToTurn .
  newForm
    [ editShowableField _TurnIndex JumpToTurn
    ]
