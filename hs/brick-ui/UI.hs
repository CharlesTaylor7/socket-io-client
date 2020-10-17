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
data Name
  deriving (Eq, Ord)

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
  , appAttrMap = const theMap
  }

handleEvent :: BrickEvent Name Tick -> AppState -> EventM Name (Next AppState)
handleEvent (AppEvent Tick) = continue
handleEvent _ = continue

drawUI :: AppState -> [Widget Name]
drawUI (history, TurnIndex turn) =
  [ center $ drawGrid game
  ]
  where
    game = history ^?! ix turn $ "history index"

drawGrid :: GameInfo -> Widget Name
drawGrid gameInfo = vBox rows
  & borderWithLabel (str "Replay")
  & withBorderStyle unicodeBold
  where
    height = gameInfo ^. #replay . #mapHeight
    width = gameInfo ^. #replay . #mapWidth
    rows =
      [ hBox $ drawRow y
      | y <- [1..height]
      ]
    drawRow y =
      [ drawTile tile
      | x <- [1..width]
      , let
          i = GridIndex $ (y - 1) * width + (x - 1)
          tile = gameInfo ^. #grid . ixGrid i
      ]

    drawTile :: Tile -> Widget Name
    drawTile tile = withAttr "emptyTile" $ str " "


theMap = attrMap V.defAttr
  [
  -- ("emptyTile", V.blue `on` V.blue)
  ]


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
