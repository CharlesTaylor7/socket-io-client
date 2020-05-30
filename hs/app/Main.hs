module Main (main) where

import Reflex
import Page.Replay
import Component.Elastic

import Data.Dom

main :: IO ()
main = mainWidget $ do
  let
    style = def
      & style_cssClass .~ Class "green-block"
      & style_inline .~ (
        def
        & at "height" ?~ "100px"
        & at "width" ?~ "100px"
        & at "background" ?~ "green"
      )

  elastic $ elStyle "div" style $ blank
