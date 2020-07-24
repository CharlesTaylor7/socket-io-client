#!/usr/bin/env stack
-- stack --resolver lts-16.6 script --package clay --package text

import Clay

import qualified ControlPanel

main = putCss $ do
  ControlPanel.styles


