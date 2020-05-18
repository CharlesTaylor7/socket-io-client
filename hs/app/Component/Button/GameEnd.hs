{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Component.Button.GameEnd where

import Types
 hiding (button)

import Data.Dom (button)
import Data.CSS


gameEndButton :: (MonadIO m, DomBuilder t m) => m ()
gameEndButton = do
  submit <- button "button-end" "End game"
  -- todo route away
  -- pure $ Setup <$ submit
  pure ()
