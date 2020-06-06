module Component.TestBlock
  ( testBlock
  ) where

import Reflex
import Component.Elastic
import Data.Dom


testBlock :: Widget t m => m ()
testBlock =
  elastic (300, 300) (0.5, 2) $ \dynStyle ->
    elDynStyle "div" (dynStyle <&> (greenBlockStyle <>)) $
      blank

greenBlockStyle :: Style
greenBlockStyle = def
  & style_cssClass .~ Class "green-block"
  & style_inline . at "background" ?~ "green"
