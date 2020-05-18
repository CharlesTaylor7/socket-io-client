{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Page.Setup where

 hiding (button)
import Types

import Data.Dom (button)

toOptions :: [Text] -> Map Int Text
toOptions names = [(0::Int)..] `zip` ("":names) & fromList

setup :: Widget t m => m ()
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

map ! i = map ^?! ix i
