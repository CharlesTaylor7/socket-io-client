{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
module ControlPanel where

import Clay
import Clay.Grid


selectReplayArea = "select-replay"
replayUrlArea = "replay-url"
selectTurnArea = "select-turn"
turnMarkerArea = "turn-marker"
perspectivesArea = "perspectives"


styles :: Css
styles = do
  ".control-panel" ? do
    position fixed
    border solid (px 1) black
    background gray
    width (px 250)
    display grid
    rowGap (px 10)
    gap (px 10) (px 10)
    padding (px 10) (px 10) (px 10) (px 10)
    justifyContent spaceBetween
    alignContent spaceBetween

    gridTemplateAreas
      [ [ selectReplayArea, replayUrlArea]
      , [ selectTurnArea, turnMarkerArea]
      , replicate 2 perspectivesArea
      ]

