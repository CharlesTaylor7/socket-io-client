{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Types
import Page.Replay.Download (download)

import Component.Grid

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  (event, triggerEvent) <- newTriggerEvent
  let trigger = liftIO . triggerEvent
  srcDyn <- holdDyn (Url "") event

  iframe srcDyn
  trigger $ Url "https://generalsio-replays-na.s3.amazonaws.com/HOVnMO6cL.gior"
  fileUploader

fileUploader :: Widget t m => m ()
fileUploader = elAttr "input" ("type" =: "file") blank



iframe :: Widget t m => Dynamic t Url -> m ()
iframe srcDyn = elDynAttr
    "iframe"
    (srcDyn <&> \(Url src) -> baseAttrs & at "src" ?~ src)
    blank
    where
      baseAttrs = "style" =:   "display:none"
