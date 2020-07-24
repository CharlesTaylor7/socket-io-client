#!/usr/bin/env stack
-- stack --resolver lts-16.6 script --package clay

import Clay
import Styles

main = putCss $ do
  sample_style


