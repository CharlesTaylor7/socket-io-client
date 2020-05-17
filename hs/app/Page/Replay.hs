{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Generals.Imports
import Generals.Types

import Js.Generals (download)
import qualified Js.FFI as FFI

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ liftIO $ do
  -- download >>= print
  blank
