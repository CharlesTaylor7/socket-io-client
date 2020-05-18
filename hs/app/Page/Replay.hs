{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

-- import Types

import Reflex hiding (button)

import Data.Dom (button)

import Page.Replay.Types
import Page.Replay.Download (download)

import Component.Grid

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  button _ _ _
  downloadEvent <- download
  widgetHold blank $ ffor downloadEvent $ \Replay{..} -> do
    grid dimensions

  blank
