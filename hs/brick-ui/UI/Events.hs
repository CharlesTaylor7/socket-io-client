module UI.Events
  ( handleEvent
  )
  where

import UI.Types

import Brick hiding (Widget, Horizontal, Vertical, Both)

import qualified Graphics.Vty as V


scrollAmount :: Int
scrollAmount = 3

handleEvent :: BrickEvent Name Tick -> AppState -> EventM Name (Next AppState)
handleEvent (AppEvent Tick) = continue
handleEvent (VtyEvent (V.EvKey key [])) =
  case key of
    V.KEsc -> halt

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
handleEvent _ = continue
