module Page.Replay.Widget.StatsPanel
  ( statsPanel
  )
  where
import Reflex

import Data.Dom

import Page.Replay.Simulate
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


statsPanel
  :: forall t m. Widget t m
  => _
  -> m _
statsPanel (replayAndGameInfoDynEvent) =
  elClass "div" "stats-panel" $
    perspectiveToggle replayAndGameInfoDynEvent

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

