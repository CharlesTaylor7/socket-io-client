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

  turnInput <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~
      ( at (toAttr "type") ?~ "text") .
      ( at (toAttr "pattern") ?~ "[0-9]*")
  let
    inputEvent = mapMaybe
      (preview (_ShowText . to JumpTo))
      (turnInput ^. inputElement_input)

  map <- toMap replay (keyEvent <> inputEvent)

  display (map ^. turn <&> \i -> "turn: " <> show i)
  void $ grid map

toAttr :: Text -> AttributeName
toAttr = AttributeName Nothing

toKey :: MonadIO m => JSVal -> m KeyCode
toKey jsval = liftIO $ do
  keyCode <- jsval ! ("keyCode" :: Text)
  fromJSValUnchecked keyCode

toCommand :: KeyCode -> Maybe Command
toCommand code =
  case keyCodeLookup code of
    KeyJ -> Just Backwards
    KeyL -> Just Forwards
    _    -> Nothing

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

  let dynTurn = view currentIndex <$> dynCache
  let dynGrid = currentGrid <$> dynCache
  pure Generals.Map
    { _tiles = dynGrid
    , _turn = dynTurn
    , _dimensions = Dimensions
        { _width  = replay ^. mapWidth
        , _height = replay ^. mapHeight
        }
    }
  where
    tiles = mountainsMap <> citiesMap <> generalsMap <> clearMap

    mountainsMap = fromList $
      [ (index, Mountain)
      | index <- replay ^. mountains
      ]

    citiesMap = fromList $
      [ (index, City (Neutral `Army` fromIntegral size))
      | (index, size) <- zip (replay ^. cities) (replay ^. cityArmies)
      ]

    generalsMap = fromList $
      [ (boardIndex, General $ Player playerId `Army` 1)
      | (playerId, boardIndex) <- replay ^.. generals . folded . withIndex
      ]

    clearMap = fromList $
      [ (i, def)
      | i <- [0..numTiles - 1]
      ]
    numTiles = replay^.mapWidth * replay^.mapHeight
