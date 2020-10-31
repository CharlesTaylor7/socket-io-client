{-# language OverloadedLists #-}
module UI.Bot (runUI) where

import Brick
import Brick.Main
import Brick.BChan
import qualified Graphics.Vty as V

import GeneralsIO
import Generals.Types

import UI.Bot.Types
import UI.Bot.Attrs (gridAttrMap)
import UI.Bot.Events (handleEvent)
import UI.Bot.Views (drawUI)


runUI :: Bot -> IO AppState
runUI bot = do
  let initialState = AppState
        { events    = []
        , bot       = bot
        , turnIndex = 0
        }
  customEventChannel <- newBChan 20
  forkIO $ do
    pure ()

    -- writeBChan customEventChannel Tick
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just customEventChannel) app initialState


app :: App AppState SocketEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const gridAttrMap
  }
