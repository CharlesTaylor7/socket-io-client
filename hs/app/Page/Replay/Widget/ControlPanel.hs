module Page.Replay.Widget.ControlPanel
  ( controlPanel
  )
  where

import Reflex

import Data.Dom

import Page.Replay.Simulate
import Page.Replay.Download (downloadReplay)
import Page.Replay.Utils (getCachedReplays)
import Page.Replay.Types

import Component.Elastic
import Component.Grid

import Js.Imports (JSVal, asyncCallback1, (!), fromJSValUnchecked)
import Js.Types
import Js.Utils
import qualified Js.FFI as FFI

import Data.Default
import Data.These


controlPanel :: Widget t m => Dynamic t Turn -> m (Event t ReplayLocation, Dynamic t Turn)
controlPanel dynMaxTurn = do
  cachedReplays :: Event t [ReplayLocation] <-
    getCachedReplays

  dropdownSelection :: Event t ReplayLocation <-
    bindEvent cachedReplays $ \replays ->
      let
        optionsVector :: Vector ReplayLocation
        optionsVector = fromList replays

        optionsMap :: Map Int Text
        optionsMap = fromList $ replays ^.. ifolded . to toDescription . withIndex

        initialKey = -1

        lookup i = optionsVector ^? ix i
      in
        dropdown initialKey (pure optionsMap) def
        <&> fmapMaybe lookup . _dropdown_change

  widgetHold_ blank $ dropdownSelection <&> \replay ->
      elAttr "a"
        ( "href" =:
          ("http://generals.io/replays/" <> replay ^. replayLocation_id)
        ) $
        text "Replay"

  -- Register j & k key commands
  (rawEvent, trigger) <- newTriggerEvent

  jsCallback <- liftIO $ asyncCallback1 trigger
  liftIO $ FFI.registerOnKeydown jsCallback

  keyEvent <- (performEvent $ rawEvent <&> toKey) <&> mapMaybe toCommand

  -- register input element
  jumpToTurnEvent <- jumpToTurnInputEl
  let commandEvent = keyEvent <> jumpToTurnEvent

  let minTurn = 0

  dynTurn <- foldDyn (commandReducer minTurn) minTurn $
    alignEventWithMaybe (Just . theseToDefaults) commandEvent $ updated dynMaxTurn

  elClass "div" "turn-marker" $
    dynText (dynTurn <&> ("turn: " <>) . show . halfRoundUp . view _Turn)

  pure (dropdownSelection, dynTurn)

jumpToTurnInputEl :: Widget t m => m (Event t Command)
jumpToTurnInputEl = do
  turnInput <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~
      ( at (toAttr "type") ?~ "text") .
      ( at (toAttr "pattern") ?~ "[0-9]*")
      -- ( at (toAttr "autofocus") ?~ "")

  let
    inputEvent = mapMaybe
      (preview $ to readEither . _Right . re (_JumpTo . _Turn))
      (turnInput ^. inputElement_input)

  pure inputEvent

toAttr :: Text -> AttributeName
toAttr = AttributeName Nothing


toDescription :: ReplayLocation -> Text
toDescription (ReplayLocation server id) =
  describe server
  <> "-"
  <> id
  where
    describe Server_Main = "main"
    describe Server_Bot = "bot"


halfRoundUp :: Int -> Int
halfRoundUp = uncurry (+) . (`divMod` 2)

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

commandReducer :: Turn -> (Command, Turn) -> Turn -> Turn
commandReducer minTurn (command, maxTurn) =
  case command of
    DoNothing -> min maxTurn
    Backwards -> max minTurn . min maxTurn . subtract 1
    Forwards  -> min maxTurn  . (+ 1)
    JumpTo n  -> const $ max minTurn $ min maxTurn n

theseToDefaults :: (Default a, Default b) => These a b -> (a, b)
theseToDefaults (This a)    = (a  , def)
theseToDefaults (That b)    = (def, b  )
theseToDefaults (These a b) = (a  , b  )
