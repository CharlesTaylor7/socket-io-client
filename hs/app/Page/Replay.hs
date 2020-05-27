module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Cache (toHistory)
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
  replayEvent <- downloadReplay replayLocation2
  widgetHold blank $ replayEvent <&> gameReplay
  blank
  where
    replayLocation1 = ReplayLocation
      { _id = "HOVnMO6cL"
      , _server = Server_Main
      }
    replayLocation2 = ReplayLocation
      { _id = "H9YxQHhiI"
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
  dynText (map ^. turn <&> ("turn: " <>) . show . uncurry (+) . (`divMod` 2))

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


commandReducer :: (Int, Int) -> Command -> Int -> Int
commandReducer (firstTurn, lastTurn) command =
  case command of
    Backwards -> max firstTurn . subtract 1
    Forwards  -> min lastTurn  . (+ 1)
    JumpTo n  -> const $ max firstTurn $ min lastTurn n

toMap
  :: (Reflex t, MonadFix m, MonadHold t m)
  => Replay
  -> Event t Command
  -> m (Generals.Map t)
toMap replay commandEvent = do
  let history = toHistory replay
  let minTurn = 0
  let maxTurn = history & length & subtract 1
  dynTurn <- foldDyn (commandReducer (minTurn, maxTurn)) 0 commandEvent

  let dynGrid = dynTurn <&> (\i -> history ^?! ix i)
  let map = Generals.Map
        { _tiles = dynGrid
        , _turn = dynTurn
        , _dimensions = Dimensions
            { _width  = replay ^. mapWidth
            , _height = replay ^. mapHeight
            }
        }
  pure map
