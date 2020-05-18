{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex
import Types

import Page.Replay.Types
import Page.Replay.Internals (download)

import Component.Grid

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  downloadEvent <- download
  widgetHold blank $ ffor downloadEvent $ \Replay{..} -> do
    grid dimensions

  blank
