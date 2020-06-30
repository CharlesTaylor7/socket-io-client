module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Simulate (toHistory)
import Page.Replay.Download (downloadReplay)
import Page.Replay.Utils (getCachedReplays)
import Page.Replay.Types
import Page.Replay.Widget.ControlPanel

import Component.Elastic
import Component.Grid

import Js.Imports (JSVal, asyncCallback1, (!), fromJSValUnchecked)
import Js.Types
import Js.Utils
import qualified Js.FFI as FFI

import Data.Default
import Control.Lens.Unsafe ((^?!))
import Data.Vector (Vector)

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


replay :: Widget t m => m ()
replay =
  elClass "div" "replay" $ do
    rec
      (replayLocationEvent, dynTurn) <- controlPanel dynMaxTurn

      replayEvent :: Event t Replay <-
        bindEvent replayLocationEvent downloadReplay

      dynMaxTurn <-
        widgetHold (pure 0) $
          replayEvent <&> gameReplay dynTurn

    blank

gameReplay :: Widget t m => Dynamic t Turn -> Replay -> m Turn
gameReplay dynTurn replay = do

  history <- toHistory replay

  let
    turnIndex = ix . view _Turn
    dynGrid = dynTurn
      <&> (\i -> history ^?! turnIndex i
          $ "history index: " <> show i
          )
    map = Generals.Map
      { _map_tiles = dynGrid
      , _map_width = replay ^. replay_mapWidth
      , _map_height = replay ^. replay_mapHeight
      }

    mapWidth = map ^. map_width . to fromIntegral
    mapHeight = map ^. map_height . to fromIntegral

    minTileSize :: Pixels
    minTileSize = 15
    initialSize = 4 * minTileSize

  elastic
    (initialSize * mapWidth, initialSize * mapHeight)
    (0.25, 2)
    (gridDynStyle map)

  let maxTurn = history & length & subtract 1 & Turn
  pure maxTurn

