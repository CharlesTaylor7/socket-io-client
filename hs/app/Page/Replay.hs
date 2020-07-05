module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Simulate (toHistory)
import Page.Replay.Download (downloadReplay)
import Page.Replay.Types
import Page.Replay.Widget.ControlPanel

import Component.Elastic
import Component.Grid

import Js.Types
import Js.Utils
import qualified Js.FFI as FFI

import Control.Lens.Unsafe ((^?!))
import Data.Vector (Vector)

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


type History = Vector Grid

replayGrid :: Widget t m => Replay -> History -> Dynamic t Turn -> m ()
replayGrid replay history dynTurn = do
  let
    turnIndex :: Turn -> Traversal' History Grid
    turnIndex = ix . view _Turn

    -- dynGrid :: Dynamic t Grid
    dynGrid = dynTurn
      <&> (\i -> history ^?! turnIndex i
          $ "history index: " <> show i
          )

    -- map :: Generals.Map t
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

  blank


replay :: Widget t m => m ()
replay =
  elClass "div" "replay" $ do
    rec
      (replayLocationEvent, dynTurn) <-
        controlPanel dynMaxTurn

      replayEvent :: Event t Replay <-
        bindEvent replayLocationEvent downloadReplay

      let
        historyEvent :: _
        historyEvent =
          pushAlways toHistory replayEvent

      let
        toMaxTurn = Turn . subtract 1 . length

        alignAlways :: Reflex t => Event t a -> Event t b -> Event t (a, b)
        alignAlways = alignEventWithMaybe $ \(These a b) -> Just (a, b)

      dynMaxTurn <-
        holdDyn 0 $ fmapCheap toMaxTurn historyEvent

    widgetHold_ blank $
      (alignAlways replayEvent historyEvent) <&> \(replay, history) -> do
        replayGrid replay history dynTurn


