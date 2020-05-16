{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Page.Game where

import Generals.Imports
import Generals.Types
import Data.Dom (button)
import Page.Game.Grid
import Js.SocketIO
import Component.Button.GameEnd

game :: [BotName] -> Widget m ()
game bots = do
  grid

  submit <- gameEndButton
  -- todo
  -- route to setup page
  pure ()
