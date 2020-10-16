module UI where

import Prelude hiding (Empty, on)
import Types

import Brick
  ( App(..)
  , AttrMap
  , BrickEvent(..)
  , EventM
  , Next
  , Widget
  , customMain
  , neverShowCursor
  , continue
  , halt
  , hLimit
  , vLimit
  , vBox
  , hBox
  , padRight
  , padLeft
  , padTop
  , padAll
  , Padding(..)
  , withBorderStyle
  , str
  , attrMap
  , withAttr
  , emptyWidget
  , AttrName
  , on
  , fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Widgets.Center as C

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

type AppState = Replay

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


drawUI :: Replay -> [Widget Name]
drawUI game =
  [ C.center $ padRight (Pad 2) (drawStats game) <+> drawGrid game
  ]

drawStats game = hLimit 11 $ vBox
  [ game ^. #score . to drawScore
  , padTop (Pad 2) $ game ^. #dead . to drawGameOver
  ]


drawScore score =
  show score
  & str
  & padAll 1
  & C.hCenter
  & B.borderWithLabel (str "Score")
  & withBorderStyle BorderStyle.unicodeBold

drawGameOver True = withAttr "gameOver" $ C.hCenter $ str "GAME OVER"
drawGameOver False = emptyWidget


drawGrid game =
  vBox rows
  & B.borderWithLabel (str "Snake")
  & withBorderStyle BorderStyle.unicodeBold
  where
    rows =
      [ hBox $ cellsInRow y
      | y <- [height-1, height-2..0]
      ]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` game ^. #snake = Snake
      | c == game ^. #food      = Food
      | otherwise               = Empty

drawCell Snake = withAttr "snakeAttr" $ str "  "
drawCell Food = withAttr "foodAttr" $ str "  "
drawCell Empty = withAttr "emptyAttr" $ str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ ("snakeAttr", V.blue `on` V.blue)
  , ("foodAttr", V.red `on` V.red)
  , ("gameOverAttr", fg V.red `V.withStyle` V.bold)
  ]
