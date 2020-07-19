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
import Page.Replay.Simulate.Types (History, GameInfo, gameInfo_grid)


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
    gameInfoDyn :: Dynamic t GameInfo
    gameInfoDyn = turnDyn
      <&> (\(Turn i) -> history ^?! ix i
          $ "history index: " <> show i
          )

    gridDyn :: Dynamic t GameInfo
    gridDyn =
      zipDynWith applyPerspective gridInfoDyn perspectiveDyn

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
  :: Perspective
  -> GameInfo
  -> Grid
applyPerspective Global gameInfo =
  gameInfo ^. gameInfo_grid

applyPerspective (Perspective playerId) gameInfo =
  let
    -- check tile & each of its 8 neighbors in the player owned cached
    isfog = undefined
  in
    -- enumerate fog tiles & mark them as such
    undefined


