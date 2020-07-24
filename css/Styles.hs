module Styles where

import Clay

sample_style = body ? do
    background  black
    color       green
    border      dashed (px 3) yellow

