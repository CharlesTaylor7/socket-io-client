module UI.Bot.Events
  ( handleEvent
  )
  where

import Generals.Types
import UI.Bot.Types

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick.Forms as Brick

import qualified Graphics.Vty as V


scrollAmount :: Int
scrollAmount = 3

handleEvent :: BrickEvent Name SocketEvent -> AppState -> EventM Name (Next AppState)
handleEvent (AppEvent js) = \s -> do
  s
  & events %~ (`snoc` js)
  & continue

handleEvent e@(VtyEvent (V.EvKey key [])) =
  case key of
    V.KEsc -> halt

    -- scroll keys
    V.KChar 'h' -> \s -> do
      hScrollBy (viewportScroll GridView) (-scrollAmount)
      continue s

    V.KChar 'l' -> \s -> do
      hScrollBy (viewportScroll GridView) (scrollAmount)
      continue s

    V.KChar 'j' -> \s -> do
      vScrollBy (viewportScroll GridView) (scrollAmount)
      continue s

    V.KChar 'k' -> \s -> do
      vScrollBy (viewportScroll GridView) (-scrollAmount)
      continue s
    _ -> continue
handleEvent e = continue
