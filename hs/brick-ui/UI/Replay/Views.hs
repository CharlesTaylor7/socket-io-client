module UI.Replay.Views
  ( drawUI
  )
  where

import Prelude hiding (Empty, on)
import Generals.Types
import Control.Lens.Unsafe

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick as Scroll (ViewportType(..))

import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Forms

import Brick.Grid (GridStyle(..), Padding(..))
import qualified Brick.Grid as Grid

import qualified Data.Text as T

import UI.Replay.Types
import UI.Replay.Attrs (ownerAttr, terrainAttr)

import qualified Data.IntSet as Set


drawUI :: AppState -> [Widget]
drawUI appState =
  [ playerStats <+> ( hCenter $ header <=> grid)
  ]
  where
    header = drawHeader appState
    playerStats = drawPlayerStats appState
    grid = drawGrid appState

-- | Draw tile
drawTile :: Tile -> (Text, AttrName)
drawTile tile = do
  let cellWidth = 4
  (tile ^. contents cellWidth, (ownerAttr <> terrainAttr) tile)
  where
    contents w =
      (_Army . #size . from (non 0) . _Just . to showArmyCount)
      `failing`
      (#_Mountain . like " ^^ ")
      `failing`
      (#_Swamp . like " vv ")
      `failing`
      (#_Fog_Clear . like " ~~ ")
      `failing`
      (#_Fog_Obstacle . like "~^^~")
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

drawHeader :: AppState -> Widget
drawHeader = do
  width <- view $ #replay . #mapWidth
  height <- view $ #replay . #mapHeight
  jumpToTurnForm <- view #jumpToTurnForm

  let
    dimensions = show width <> "x" <> show height
    titleWidget = txt $ "Replay " <> dimensions
    paddingWidget = txt $ T.replicate 20 " "
    turnInputWidget =
      (txt ("Jump to: ") <+>) $
      hLimit 10 $
      renderForm $
        jumpToTurnForm

  pure $
    hCenter $
      titleWidget <+> paddingWidget <+> turnInputWidget


drawGrid :: AppState -> Widget
drawGrid = do
  game <- asks currentGame
  let
    width = game ^. #replay . #mapWidth
    height = game ^. #replay . #mapHeight

    toTile :: (Int, Int) -> Tile
    toTile (i, j) = game ^?! #grid . #_Grid . ix (j * width + i) $ "grid index"

    gridStyle = GridStyle
      { borderStyle = unicode
      , gridWidth = width
      , gridHeight = height
      , padding = PadLeft
      }

    gridContent = Grid.drawGrid (drawTile . toTile) gridStyle

  pure $
    viewport GridView Scroll.Vertical $
    hCenter $
    cached GridView $
      gridContent


drawPlayerStats :: AppState -> Widget
drawPlayerStats = do
  usernames <- view $ #replay . #usernames
  game <- asks currentGame
  let
    owned = game ^. #owned
    grid  = game ^. #grid

    statsGrid :: Map (Int, Int) (Text, AttrName)
    statsGrid = flip execState mempty $ do
      ifor_ headers $ \i h ->
        at (i, 0) ?= (h, mempty)

      for_ [1 .. length usernames] $ \j -> do
        at (0, j) ?= getPlayerName (j - 1)
        at (1, j) ?= getTileCount (j - 1)
        at (2, j) ?= getArmyCount (j - 1)
        at (3, j) ?= getKilledBy (j - 1)

    headers = ["username", "tiles", "armies", "killed by"]

    getPlayerName i =
      ( usernames ^?! ix i $ "player index"
      , (attrName $ "player" <> show (i + 1))
        <> if (is _Nothing $ owned ^? at i) then "dead" else "alive"
      )

    getTileCount i =
      (owned ^. at i . non mempty . to Set.size . to show , mempty)

    getArmyCount i =
      (show $ totalArmies i, mempty)

    totalArmies playerId =
      owned
        & sumOf
          ( ix playerId
          . folding Set.toList
          . to (\i -> grid ^? #_Grid . ix i)
          . _Just
          . _Army
          . #size
          )

    getKilledBy i = do
      let kill = game ^? #kills . folded . filtered ((== i) . (view #mark))
      case kill of
        Just k ->
          ( k ^. #turn . to (\t -> "turn " <> show t)
          , attrName ("player" <> k ^. #killer . to (+1) . to show) <> "general"
          )
        Nothing -> mempty

    toTile = \c -> statsGrid ^?! ix c $ "index"

    gridStyle = GridStyle
      { borderStyle = unicodeRounded
      , gridWidth = 4
      , gridHeight = 1 + (length usernames)
      , padding = PadRight
      }

  pure $
    Grid.drawGrid toTile gridStyle
