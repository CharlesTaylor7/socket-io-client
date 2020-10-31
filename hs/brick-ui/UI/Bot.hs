{-# language OverloadedLists #-}
module UI.Bot (runUI) where

import Brick.Main
import Brick.BChan
import qualified Graphics.Vty as V

import qualified GeneralsIO as G
import Generals.Types

import UI.Bot.Types
import UI.Bot.Attrs (gridAttrMap)
import UI.Bot.Events (handleEvent)
import UI.Bot.Views (drawUI)

import qualified Pipes


runUI :: G.Bot -> G.GameServer -> IO AppState
runUI bot gameServer = do
  -- connect to the generals server
  (client, eventStream) <- G.connect

  -- send the server events through the brick side channel
  customEventChannel <- newBChan 20
  forkIO $ Pipes.runEffect $ do
    Pipes.for eventStream $ \(event:: SocketEvent) ->
      liftIO $ writeBChan customEventChannel event

  -- setup initial app state
  let initialState = AppState
        { events    = []
        , turnIndex = 0
        , bot
        , gameServer
        }

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
