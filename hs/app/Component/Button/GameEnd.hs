{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
module Component.Button.GameEnd where


import Data.Dom (button)
import Data.CSS (ToText(..))
import Frontend.Types
import Js.Utils (window_confirm)

gameEndButton :: JS_Widget js t m ()
gameEndButton = do
  submit <- button "button-end" "End game"
  window_confirm "Are you sure" undefined
  -- todo route away
  -- pure $ Setup <$ submit
  pure ()
