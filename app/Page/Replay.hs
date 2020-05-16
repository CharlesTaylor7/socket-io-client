{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Frontend.Imports
import Frontend.Types

import Js.SocketIO (Url(..))
import qualified Js.SocketIO as SIO

import Js.Generals

replay :: JS_Widget js t m ()
replay = elClass "div" "replay" $ do
  text "hello!"
  download
  blank
