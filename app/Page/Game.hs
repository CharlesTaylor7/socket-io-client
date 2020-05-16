{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Page.Game where

import Reflex.Dom hiding (button)

import Common.Types
import Frontend.Types
import Data.Dom (button)
import Page.Game.Grid
import Js.SocketIO
import Component.Button.GameEnd

game :: [BotName] -> JS_Widget js t m ()
game bots = do
  grid

  submit <- gameEndButton
  -- todo
  -- route to setup page
  pure ()
