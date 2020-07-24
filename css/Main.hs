#!/usr/bin/env stack
{-
  stack script
  --resolver lts-16.6
  --package clay
  --package text
-}

import Clay

import qualified ControlPanel

main = putCss $ do
  ControlPanel.styles


