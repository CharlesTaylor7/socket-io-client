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
  (client, eventStream) <- G.connect

  -- send the server events through the brick side channel
  customEventChannel <- newBChan 20
  forkIO $ Pipes.runEffect $ do
    Pipes.for eventStream $ \(event :: SocketEvent) ->
      liftIO $ writeBChan customEventChannel event

  -- setup initial app state
  let initialState = AppState
        { events    = []
        , turnIndex = 0
        , bot
        , gameConfig
        , client
        }

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just customEventChannel) app initialState


startEvent :: AppState -> EventM n AppState
startEvent appState = do
  G.register (appState ^. #bot) (appState ^. #client)
  pure appState


app :: App AppState SocketEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = startEvent
  , appAttrMap = const gridAttrMap
  }
