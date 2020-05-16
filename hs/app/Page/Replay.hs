{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Generals.Imports
import Generals.Types

import Js.SocketIO (Url(..))
import qualified Js.SocketIO as SIO

import Js.Generals

replay :: Widget m ()
replay = elClass "div" "replay" $ do
  text "hello!"
  download
  blank
