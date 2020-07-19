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

import Generals.Map.Types
import Page.Replay.Simulate.Types hiding (Turn, Turns)

import Control.Lens.Unsafe ((^?!))


controlPanel
  :: forall t m. Widget t m
  => m _
controlPanel =
  elClass "div" "control-panel" $ do
    rec
      -- dom elements
      replayLocationEvent :: Event t ReplayLocation <-
        replayDropdown cachedReplays

      replayUrlHref replayLocationEvent

      jumpToTurnEvent :: Event t Command <-
        jumpToTurnInputEl

      turnMarker turnDyn

      perspectiveEvents <-
        perspectiveToggle replayAndGameInfoDynEvent

      -- effects
      cachedReplays :: Event t [ReplayLocation] <-
        getCachedReplays

      keyEvent :: Event t Command <-
        registerKeyCommands

      turnDyn :: Dynamic t Turn <-
        buildDynTurn commandEvent dynMaxTurn

      dynMaxTurn <-
        holdDyn 0 $ fmapCheap toMaxTurn historyEvent

      replayEvent :: Event t Replay <-
        bindEvent replayLocationEvent downloadReplay

      perspectiveToggleDyn :: Dynamic t Perspective <-
        holdDyn Global $
        switchDyn perspectiveEvents

      gameInfoEvent :: Event t GameInfo <-
          replayAndGameInfoDynEvent
          & fmapCheap (updated . snd)
          & switchEvent

      changePerspectiveDueToKillDynamic :: Dynamic t (Maybe Perspective) <-
        holdMaybe $
        flip push gameInfoEvent
          (\gameInfo -> do
            p :: Perspective <-
              sample $ current perspectiveToggleDyn
            pure $ do
              playerId <- p ^? _Perspective
              if is _Just $ gameInfo ^? gameInfo_owned . ix playerId
              then Nothing
              else do
                let
                  filter :: Kill -> Bool
                  filter = (== playerId) . view kill_target

                  answer =  gameInfo
                    ^?! (gameInfo_kills . folded . filtered filter . kill_killer . to Perspective)
                    $ "Expected kill for perspective shift"
                Just answer
          )
      let
        replayAndGameInfoDynEvent :: Event t (Replay, Dynamic t GameInfo)
        replayAndGameInfoDynEvent =
          unsafeAlignWith combine replayEvent historyEvent
          where
            combine =
              \replay history ->
                ( replay
                , turnDyn <&> \(Turn turn) ->
                    history ^?! ix turn $ "history index: " <> show turn
                )

        historyEvent :: Event t History
        historyEvent =
          pushAlways toHistory replayEvent

        toMaxTurn :: History -> Turn
        toMaxTurn = Turn . subtract 1 . length

        commandEvent :: Event t Command
        commandEvent = keyEvent <> jumpToTurnEvent

        perspectiveDyn :: Dynamic t Perspective
        perspectiveDyn = zipDynWith combine perspectiveToggleDyn changePerspectiveDueToKillDynamic
          where
            combine :: Perspective -> Maybe Perspective -> Perspective
            combine _ (Just p2) = p2
            combine p1 _        = p1

    pure (replayAndGameInfoDynEvent, perspectiveDyn)

changed :: (MonadHold t m, Reflex t, Eq a) => Event t a -> m (Event t a)
changed event = do
  behavior <- hold Nothing $ fmapCheap Just event
  pure $ push
    (\new -> do
      old <- sample behavior
      if old == Just new
      then pure $ Nothing
      else pure $ Just new
    )
    event

holdMaybe :: (Reflex t, MonadHold t m) => Event t a -> m (Dynamic t (Maybe a))
holdMaybe event = holdDyn Nothing $ fmapCheap Just event
--
unsafeAlignWith :: Reflex t => (a -> b -> c) -> Event t a -> Event t b -> Event t c
unsafeAlignWith f = alignEventWithMaybe (\(These a b) -> Just (f a b))

replayDropdown :: forall t m. Widget t m => Event t [ReplayLocation] -> m (Event t ReplayLocation)
replayDropdown cachedReplays =
  bindEvent cachedReplays $
  \replays -> do
    let
      optionsVector :: Vector ReplayLocation
      optionsVector = fromList replays

      optionsMap :: Map Int Text
      optionsMap = fromList $ replays ^.. ifolded . to toDescription . withIndex

      initialKey :: Int
      initialKey = -1

      lookup :: Int -> Maybe ReplayLocation
      lookup i = optionsVector ^? ix i

      dropdownConfig :: DropdownConfig t Int
      dropdownConfig =
        def
        & dropdownConfig_attributes .~
          ( def
          & at "class" ?~ "replay-dropdown"
          & constDyn
          )

    dropdown initialKey (pure optionsMap) dropdownConfig
      <&> fmapMaybe lookup . _dropdown_change


replayUrlHref :: Widget t m => Event t ReplayLocation -> m ()
replayUrlHref i = widgetHold_ blank $ i <&>
  \replay ->
  elAttr "a"
    ( mempty
    & at "class" ?~ "replay-url"
    & at "href" ?~ "http://generals.io/replays/" <> replay ^. replayLocation_id
    & at "target" ?~ "_blank"
    ) $
    text "Replay"


jumpToTurnInputEl :: Widget t m => m (Event t Command)
jumpToTurnInputEl = do
  turnInput <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~
      ( at (toAttr "class") ?~ "jump-to-turn") .
      ( at (toAttr "type") ?~ "text") .
      ( at (toAttr "pattern") ?~ "[0-9]*")

      -- ( at (toAttr "autofocus") ?~ "")

  let
    inputEvent = mapMaybe
      (preview $ unpacked . to readEither . _Right . re (_JumpTo . _Turn))
      (turnInput ^. inputElement_input)

  pure inputEvent

  where
    toAttr :: Text -> AttributeName
    toAttr = AttributeName Nothing


registerKeyCommands :: Effects t m => m (Event t Command)
registerKeyCommands = do
-- Register j & k key commands
  (rawEvent, trigger) <- newTriggerEvent

  jsCallback <- liftIO $ asyncCallback1 trigger
  liftIO $ FFI.registerOnKeydown jsCallback

  (performEvent $ rawEvent <&> toKey) <&> mapMaybe toCommand


buildDynTurn :: forall t m. (Reflex t, MonadHold t m, MonadFix m) => Event t Command -> Dynamic t Turn -> m (Dynamic t Turn)
buildDynTurn commandEvent dynMaxTurn = do
  let
    maxTurnOrCommandEv :: Event t (Turn, Command)
    maxTurnOrCommandEv = leftmost
      [ attach (current dynMaxTurn) commandEvent
      , updated dynMaxTurn & fmapCheap (,DoNothing)
      ]
  foldDyn (commandReducer def) def $ maxTurnOrCommandEv


turnMarker :: forall t m. Widget t m => Dynamic t Turn -> m ()
turnMarker turnDyn =
  elClass "span" "turn-marker" $
  dynText (turnDyn <&> \(Turn turn) ->
    "turn: "
    <> show turn
    <> " ("
    <> show (halfRoundUp turn)
    <> ")"
  )


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

commandReducer :: Turn -> (Turn, Command) -> Turn -> Turn
commandReducer minTurn (maxTurn, command) =
  case command of
    DoNothing -> min maxTurn
    Backwards -> max minTurn . min maxTurn . subtract 1
    Forwards  -> min maxTurn  . (+ 1)
    JumpTo n  -> const $ max minTurn $ min maxTurn n

perspectiveToggle
  :: forall t m. Widget t m
  => Event t (Replay, Dynamic t GameInfo)
  -> m _
perspectiveToggle event =
  elClass "div" "perspective-toggle" $
    widgetHold (pure never) $
      (event <&> \(replay, gameInfoDyn) ->
        replay
        & toPerspectives
        & traverse
          ( \(p, t) ->
            perspectiveButton p t (gameInfoDyn <&> isDisabled p)
          )
        & fmap leftmost
      )
  where
    isDisabled :: Perspective -> GameInfo -> Bool
    isDisabled Global = const False
    isDisabled (Perspective playerId) =
      orOf
      $ gameInfo_owned
      . at playerId
      . failing _Nothing (_Just . _Empty)
      . like True

    toPerspectives :: Replay -> [(Perspective, Text)]
    toPerspectives replay =
      (Global, show Global)
      : replay
      ^.. replay_usernames
      . folded
      . withIndex
      . alongside (to Perspective) identity



perspectiveButton
  :: forall t m. Widget t m
  => Perspective
  -> Text
  -> Dynamic t Bool
  -> m (Event t Perspective)
perspectiveButton perspective label disabledDyn = do
  (buttonElement, _) <-
    elDynAttr' "button" attrs $
      text label

  pure $ domEvent Click buttonElement $> perspective
  where
    attrs = disabledDyn <&>
      \disabled -> def
        & at "class" ?~ className
        & at "disabled" .~ bool Nothing (Just "disabled") disabled

    className =
      case perspective of
        Global -> "global"
        Perspective id -> "player-" <> show id

