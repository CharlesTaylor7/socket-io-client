module UI where

import Prelude hiding (Empty, on)
import Types
import Control.Lens.Unsafe

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

import qualified Graphics.Vty as V


-- Types

-- | Ticks mark passing of time
--
-- the app's custom event type
data Tick = Tick

-- | Named resources
data Name = GridView
  deriving (Eq, Ord, Show)

data Cell
  = Snake
  | Food
  | Empty
  deriving (Eq, Show)

type AppState = (History, TurnIndex)

-- App definition
app :: App AppState Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const gridAttrMap
  }

handleEvent :: BrickEvent Name Tick -> AppState -> EventM Name (Next AppState)
handleEvent (AppEvent Tick) = continue
handleEvent (VtyEvent (V.EvKey key [])) =
  case key of
    V.KEsc -> halt
    V.KChar 'j' -> \s -> do
      vScrollBy (viewportScroll GridView) (scrollAmount)
      continue s

    V.KChar 'k' -> \s -> do
      vScrollBy (viewportScroll GridView) (-scrollAmount)
      continue s

    _ -> continue
handleEvent _ = continue

scrollAmount = 5

drawUI :: AppState -> [Widget Name]
drawUI (history, TurnIndex turn) =
  [ center $ drawGrid game
  ]
  where
    game = history ^?! ix turn $ "history index"


drawGrid :: GameInfo -> Widget Name
drawGrid gameInfo = title <=> grid
  where
    title = hCenter $ str $ "Replay " <> dimensions
    dimensions = show width <> "x" <> show height
    width = gameInfo ^. #replay . #mapWidth
    height = gameInfo ^. #replay . #mapHeight

    grid :: Widget Name
    grid =
      viewport GridView Vertical $
      hCenter $
      insertHBorders $
      [ drawRow y
      | y <- [1..height]
      ]

    drawRow :: Int -> Widget Name
    drawRow y = vLimit 1 $ insertVBorders $
      [ drawTile tile
      | x <- [1..width]
      , let
          i = GridIndex $ (y - 1) * width + (x - 1)
          tile = gameInfo ^. #grid . ixGrid i
      ]

    drawTile :: Tile -> Widget Name
    drawTile tile = str (tile ^. contents) & withAttr (tile ^. attr)
      where
        attr =
          (_Owner . #_Player . to show . to ("player" <>) . to attrName)
          `failing`
          like "neutral"
        contents =
          (_Army . #size . from (non 0) . _Just . to showArmyCount)
          `failing`
          like (replicate cellWidth ' ')


    cellWidth :: Int
    cellWidth = 4

    cellHeight :: Int
    cellHeight = 4


    vBorder = str "┃"

    insertVBorders :: [Widget Name] -> Widget Name
    insertVBorders = hBox . surround vBorder . intersperse vBorder

    insertHBorders :: [Widget Name] -> Widget Name
    insertHBorders = vBox . (hBorder Top :) . (<> [hBorder Bottom]) . intersperse (hBorder Middle)

    topBorder = hBorder Top
    bottomBorder = hBorder Bottom

    hBorder :: VLocation -> Widget Name
    hBorder v = replicate cellWidth '━'
          & replicate width
          & intercalate [(borderChar v Center)]
          & corners v
          & str

data VLocation = Bottom | Middle | Top
data HLocation = Start | Center | End

corners :: VLocation -> String -> String
corners v text = [borderChar v Start] <> text <> [borderChar v End]

borderChar :: VLocation -> HLocation -> Char
borderChar Bottom Start = '┗'
borderChar Bottom Center = '┻'
borderChar Bottom End = '┛'
borderChar Middle Start = '┣'
borderChar Middle Center = '╋'
borderChar Middle End = '┫'
borderChar Top Start = '┏'
borderChar Top Center = '┳'
borderChar Top End = '┓'

-- | show army count in 4 characters
-- if the army is over 4 digits, use k prefix
-- if the army is under 4 digits pad with space characters
showArmyCount :: Int -> String
showArmyCount n
  | n < 10 = "  " <> show n <> " "
  | n < 100 = " " <> show n <> " "
  | n < 1_000 = show n <> " "
  | n < 10_000 = show n
  | n < 100_000 = " " <> show (n `div` 1_000) <> "k"
  | n < 1_000_000 = show (n `div` 1_000) <> "k"
  | otherwise = "larj"

surround c list = c : (list <> [c])


gridAttrMap = attrMap V.defAttr
  [ ("obstacle", fg grey)
  , ("player1", fg V.brightBlue)
  , ("player2", fg V.red)
  , ("player3", fg V.green)
  , ("player4", fg purple)
  , ("player5", fg teal)
  , ("player6", fg V.yellow)
  , ("player7", fg V.cyan)
  , ("player8", fg V.magenta)
  ]
  where
    teal = V.rgbColor 0 0x80 0x80
    purple = V.rgbColor 0x80 0 0x80
    grey = V.rgbColor 0x71 0x6f 0x6f



brickMain :: History -> IO ()
brickMain history = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100_000

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) app (history, TurnIndex 0)

  pure ()
