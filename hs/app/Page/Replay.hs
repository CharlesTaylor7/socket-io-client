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

import Generals.Map.Types
import Page.Replay.Simulate.Types hiding (Turn, Turns)

import Data.Vector (Vector)
import qualified Data.IntSet as Set


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

    gridDyn :: Dynamic t Grid
    gridDyn =
      zipDynWith applyPerspective perspectiveDyn gameInfoDyn

    dimensions :: (Int, Int)
    dimensions =
      (replay ^. replay_mapWidth, replay ^. replay_mapHeight)

    minTileSize :: Pixels
    minTileSize = 15
    initialSize = 4 * minTileSize

    initialMapDimensions :: (Pixels, Pixels)
    initialMapDimensions = dimensions & both %~ (initialSize *) . fromIntegral

  elastic
    initialMapDimensions
    (0.25, 2)
    (gridDynStyle dimensions gridDyn)


applyPerspective
  :: Perspective
  -> GameInfo
  -> Grid
applyPerspective Global gameInfo =
  gameInfo ^. gameInfo_grid

applyPerspective (Perspective playerId) gameInfo =
  let
    -- check tile & each of its 8 neighbors in the player owned cached
    allTiles :: IntSet
    allTiles = fromList [0 .. gameInfo ^. gameInfo_numTiles - 1]

    owned :: IntSet
    owned = gameInfo ^. gameInfo_owned . ix playerId

    visible :: IntSet
    visible = owned ^. members . to visibleFrom . to fromList

    visibleFrom :: Int -> [Int]
    visibleFrom i =
      let
        w = gameInfo ^. gameInfo_gridWidth
        column = [i - w, i, i + w]
        next = column <&> (+ 1)
        prev = column <&> subtract 1
      in
        case i `rem` w of
          0              -> column <> next
          r | r == (w-1) -> column <> prev
          _              -> column <> next <> prev

    fog :: IntSet
    fog = Set.difference allTiles visible

    markFog :: Tile -> Tile
    markFog (Clear _)   = Fog_Clear
    markFog (General _) = Fog_Clear
    markFog Mountain    = Fog_Obstacle
    markFog (City _)    = Fog_Obstacle
    markFog (Swamp _)   = Fog_Obstacle
  in
    foldrOf
      members
      (\tile -> _Grid . ix tile %~ markFog)
      (gameInfo ^. gameInfo_grid)
      fog
