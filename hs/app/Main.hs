module Main (main) where

import Reflex
import Page.Replay
import Component.Elastic

import Data.Dom

main :: IO ()
main = mainWidget $ do
  elastic (300, 300) $ \dynStyle ->
    elDynStyle "div" (dynStyle <&> (greenBlockStyle <>)) $
      blank


greenBlockStyle :: Style
greenBlockStyle = def
  & style_cssClass .~ Class "green-block"
  & style_inline . at "background" ?~ "green"
