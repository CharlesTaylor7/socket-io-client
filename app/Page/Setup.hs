{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Page.Setup where

import Relude
import Data.Map ((!))
import qualified Data.Map as Map
import Control.Lens

import Reflex.Dom hiding (Widget, button)

import Data.Dom (button)
import Frontend.Types

toOptions :: [Text] -> Map Int Text
toOptions names = [(0::Int)..] `zip` ("":names) & Map.fromAscList

setup :: JS_Widget js t m ()
setup = elClass "div" "setup" $ do
  let optionsDyn = constDyn optionsMap
  dropdown1 <- mkDropdown 1 optionsDyn "bot-selector"
  dropdown2 <- mkDropdown 2 optionsDyn "bot-selector"

  let namesBehavior = combineDropdowns optionsMap
        [dropdown1, dropdown2]
  let buildGamePage = attachWith (const . GameConfig . map BotName) namesBehavior
  buildGamePage <$> button "button-start" "Start game"
  pure ()

mkDropdown initial dynOptions className =
      dropdown initial dynOptions $
        def
        & dropdownConfig_attributes .~ constDyn ("class" =: className)

botNames = ["turtle", "chuck"] :: [Text]
optionsMap = toOptions botNames


combineDropdowns :: (Reflex t, Ord k) => Map k v -> [Dropdown t k] -> Behavior t [v]
combineDropdowns = traverse . toBehavior

toBehavior :: (Reflex t, Ord k) => Map k v -> Dropdown t k -> Behavior t v
toBehavior map = fmap (map !) . current . _dropdown_value
