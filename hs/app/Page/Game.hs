{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Page.Game where

import Types
import Data.Dom (button)
import Component.Grid
import Js.SocketIO
import Component.Button.GameEnd

game :: Widget t m => [BotName] -> m ()
game bots = do
  grid
  submit <- gameEndButton
  -- todo
  -- route to setup page
  pure ()
