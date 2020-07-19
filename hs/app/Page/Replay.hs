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
import Page.Replay.Simulate.Types (History, gameInfo_grid)


replay :: forall t m. Widget t m => m ()
replay =
  elClass "div" "replay" $ do
    (replayAndHistoryEvent, turnDyn, perspectiveDyn) <-
      controlPanel

    widgetHold_ blank $
      replayAndHistoryEvent <&> \(replay, history) -> do
        replayGrid replay history turnDyn perspectiveDyn


replayGrid
  :: forall t m. Widget t m
  => Replay
  -> History
  -> Dynamic t Turn
  -> Dynamic t Perspective
  -> m ()
replayGrid replay history turnDyn perspectiveDyn = do
  let
    gridDyn :: Dynamic t Grid
    gridDyn = turnDyn
      <&> (\(Turn i) -> history ^?!  ix i . gameInfo_grid
          $ "history index: " <> show i
          )

    gridWithPerspective :: Dynamic t Grid
    gridWithPerspective =
      zipDynWith applyPerspective gridDyn perspectiveDyn

    dimensions :: (Int, Int)
    dimensions =
      (replay ^. replay_mapWidth, replay ^. replay_mapHeight)

    minTileSize :: Pixels
    minTileSize = 15
    initialSize = 4 * minTileSize

  elastic
    (dimensions & both %~ (initialSize *) . fromIntegral)
    (0.25, 2)
    (gridDynStyle dimensions gridWithPerspective)


applyPerspective
  :: Grid
  -> Perspective
  -> Grid
applyPerspective grid perspective = grid

