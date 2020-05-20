{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Types
import Page.Replay.Download (download)
import Js.Utils

import Component.Grid
import Component.FileUpload
import Component.FileDownload

import Prelude hiding ((#), (!), (!!))

import Js.Imports
import Js.Types
import qualified Js.FFI as FFI

import Data.Default
import Data.Default.Orphans

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  replayEvent <- download
  widgetHold_ blank $ replayEvent <&> showReplay
  blank


showReplay :: Widget t m => Replay -> m ()
showReplay Replay {..} = text $ show generals
