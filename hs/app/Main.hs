module Main (main) where

import Reflex
import Page.Replay
import Component.Elastic

import Data.Dom

main :: IO ()
main = mainWidget $ do
  elastic $ \dynStyle ->
    elDynStyle "div" (dynStyle <&> (greenBlockStyle <>)) $
      blank


greenBlockStyle :: Style
greenBlockStyle = def
  & style_cssClass .~ Class "green-block"
  & style_inline .~ (
    def
    & at "height" ?~ "100px"
    & at "width" ?~ "100px"
    & at "background" ?~ "green"
  )
