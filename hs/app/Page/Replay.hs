module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Simulate
import Page.Replay.Download
import Page.Replay.Utils (getCachedReplays)
import Page.Replay.Types

import Component.Elastic
import Component.Grid

import Js.Imports (JSVal, asyncCallback1, (!), fromJSValUnchecked)
import Js.Types
import Js.Utils
import qualified Js.FFI as FFI

import Data.Default
import Data.Vector (Vector)

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals



replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  replaysEvent :: Event t [ReplayLocation] <- getCachedReplays

  dropdownSelection :: Event t ReplayLocation <-
    fmap switchDyn $
    widgetHold (pure def) $
    replaysEvent <&> \replays ->
      let
        optionsVector = fromList replays :: Vector ReplayLocation
        optionsMap = fromList $ replays ^.. ifolded . to toDescription . withIndex
        initialKey = 0 :: Int
        lookup i = optionsVector ^?! ix i
      in
        dropdown initialKey (pure optionsMap) def
        <&> fmap lookup . _dropdown_change



  -- widgetHold blank $ dropdownSelection
  -- replayEvent <- downloadReplay replayLocation2
  -- widgetHold blank $ replayEvent <&> gameReplay
  blank




toDescription :: ReplayLocation -> Text
toDescription (ReplayLocation server id) =
  describe server
  <> "-"
  <> show id
  where
    describe Server_Main = "main"
    describe Server_Bot = "bot"


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
      ( at (toAttr "pattern") ?~ "[0-9]*") .
      ( at (toAttr "autofocus") ?~ "")
  let
    inputEvent = mapMaybe
      (preview (to readEither . _Right . to JumpTo))
      (turnInput ^. inputElement_input)

  map <- toMap replay (keyEvent <> inputEvent)
  elClass "div" "turn-marker" $
    dynText (map ^. map_turn <&> ("turn: " <>) . show . halfRoundUp)


  let
    mapWidth = map ^. map_width . to fromIntegral
    mapHeight = map ^. map_height . to fromIntegral

    minTileSize :: Pixels
    minTileSize = 15
    initialSize = 4 * minTileSize

  elastic
    (initialSize * mapWidth, initialSize * mapHeight)
    (0.25, 2)
    (gridDynStyle map)

halfRoundUp :: Int -> Int
halfRoundUp = uncurry (+) . (`divMod` 2)

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
        { _map_tiles = dynGrid
        , _map_turn = dynTurn
        , _map_width = replay ^. replay_mapWidth
        , _map_height = replay ^. replay_mapHeight
        }
  pure map
