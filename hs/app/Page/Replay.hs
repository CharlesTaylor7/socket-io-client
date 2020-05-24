module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Cache (commandReducer, newCache, currentGrid, toTurns)
import Page.Replay.Download
import Page.Replay.Types

import Component.Grid
import Component.FileUpload
import Component.FileDownload

import Prelude hiding ((#), (!), (!!))

import Js.Imports
import Js.Types
import Js.Utils
import qualified Js.FFI as FFI

import Data.Default
import Data.Default.Orphans

import Types (Dimensions(..))

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  replayEvent <- downloadReplay replayLocation
  widgetHold blank $ replayEvent <&> gameReplay
  blank
  where
    replayLocation = ReplayLocation
      { _id = "HOVnMO6cL"
      , _server = Server_Main
      }

gameReplay :: Widget t m => Replay -> m ()
gameReplay replay = do
  (rawEvent, trigger) <- newTriggerEvent

  jsCallback <- liftIO $ asyncCallback1 trigger
  liftIO $ FFI.registerOnKeydown jsCallback

  keyEvent <- (performEvent $ rawEvent <&> toKey) <&> mapMaybe toCommand
  map <- toMap replay keyEvent
  void $ grid map


toKey :: MonadIO m => JSVal -> m KeyCode
toKey jsval = liftIO $ do
  keyCode <- jsval ! ("keyCode" :: Text)
  fromJSValUnchecked keyCode

toCommand :: KeyCode -> Maybe Command
toCommand code =
  case keyCodeLookup code of
    KeyJ  -> Just Backwards
    KeyL -> Just Forwards
    _          -> Nothing

toMap
  :: (Reflex t, MonadFix m, MonadHold t m,
    -- debug constraints
    MonadIO m, PostBuild t m, DomBuilder t m)
  => Replay
  -> Event t Command
  -> m (Generals.Map t)
toMap replay commandEvent = do
  let seed = newCache tiles
  let turns = toTurns replay
  dynCache <- foldDyn (commandReducer (replay, turns)) seed commandEvent
  display (dynCache <&> \cache ->
      "turn: " <> cache^.currentIndex.re _Show
    )
  let dynGrid = currentGrid <$> dynCache
  pure $ Generals.Map
    { _tiles = dynGrid
    , _dimensions = Dimensions
        { _width  = replay ^. mapWidth
        , _height = replay ^. mapHeight
        }
    }
  where
    toCoord :: Int -> (Int, Int)
    toCoord = view $ coordinated (replay ^. mapWidth)

    tiles = mountainsMap <> citiesMap <> generalsMap <> clearMap

    mountainsMap = fromList $
      [ (toCoord index, Mountain)
      | index <- replay ^. mountains
      ]
    citiesMap = fromList $
      [ (toCoord index, City (Neutral `Army` fromIntegral size))
      | (index, size) <- zip (replay ^. cities) (replay ^. cityArmies)
      ]
    generalsMap = fromList $
      [ (toCoord boardIndex, General $ Player playerId `Army` 1)
      | (playerId, boardIndex) <- replay ^.. generals . folded . withIndex
      ]
    clearMap = fromList $
      [((i, j), def)
      | i <- [1..replay ^. mapWidth]
      , j <- [1..replay ^. mapHeight]
      ]
