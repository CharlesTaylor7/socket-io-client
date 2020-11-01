{-# language OverloadedLists #-}
module UI.Bot (runUI) where

import Brick.Main
import Brick.BChan
import Brick.Types (EventM)
import qualified Graphics.Vty as V

import qualified GeneralsIO as G
import Generals.Types

import UI.Bot.Types
import UI.Bot.Attrs (gridAttrMap)
import UI.Bot.Events (handleEvent)
import UI.Bot.Views (drawUI)

import qualified Pipes


runUI :: G.Bot -> G.GameConfig -> IO AppState
runUI bot gameConfig = do
  -- connect to the generals server
  (client, eventStream, errorStream) <- G.connect

  -- send the server events through the brick side channel
  customEventChannel <- newBChan 20

  forkIO $ Pipes.runEffect $
    Pipes.for errorStream $ \event -> liftIO $ do
      writeBChan customEventChannel (ErrorEvent event)

  forkIO $ Pipes.runEffect $
    Pipes.for eventStream $ \event -> liftIO $ do
      writeBChan customEventChannel (GameEvent event)

  -- setup initial app state
  let appState = AppState
        { events    = []
        , turnIndex = 0
        , bot
        , gameConfig
        , client
        }

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just customEventChannel) app appState


startEvent :: AppState -> EventM n AppState
startEvent appState = do
  let client = appState ^. #client
  client & G.register (appState ^. #bot)
  -- client & G.join (appState ^. #gameConfig) (appState ^. #bot)
  pure appState


app :: App AppState AppEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = startEvent
  , appAttrMap = const gridAttrMap
  }
