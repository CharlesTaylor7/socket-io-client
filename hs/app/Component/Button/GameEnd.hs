{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Component.Button.GameEnd where

import Generals.Types
import Generals.Imports hiding (button)

import Data.Dom (button)
import Data.CSS

import Js.Utils (window_confirm)

gameEndButton :: (MonadIO m, DomBuilder t m) => m ()
gameEndButton = do
  submit <- button "button-end" "End game"
  window_confirm "Are you sure" undefined
  -- todo route away
  -- pure $ Setup <$ submit
  pure ()
