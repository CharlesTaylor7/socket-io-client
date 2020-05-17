{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Generals.Imports
import Generals.Types

import Js.Generals (download)

replay :: Widget m ()
replay = elClass "div" "replay" $ do
  downloaded <- download
  print downloaded
  blank
