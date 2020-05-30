module Component.Elastic
  ( elastic
  ) where

import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

type Dragging = All
pattern DragOn  = All True
pattern DragOff = All False


elastic
  :: forall t m a.
  (  Widget t m
  )
  => m a
  -> m a
elastic child = do
  rec
    -- elastic wrapper
    (e, a) <- elDynStyle' "div" dynStyle $ child

    dragBehavior <- hold DragOff toggleDragEvent

    dynStyle <-holdDyn def $ fmapCheap toStyle dragEvent

    let
      mouseDown, mouseUp, mouseLeave :: Event t Dragging
      mouseDown = domEvent Mousedown e $> DragOn
      mouseUp = domEvent Mouseup e $> DragOff
      mouseLeave = domEvent Mouseleave e $> DragOff

      toggleDragEvent :: Event t Dragging
      toggleDragEvent = mouseDown <> mouseUp <> mouseLeave

      dragEvent :: Event t (Int, Int)
      dragEvent = gate (coerceBehavior dragBehavior) moveEvent
        where
          moveEvent = domEvent Mousemove e

  pure a

toStyle :: (Int, Int) -> Style
toStyle (mouseX, mouseY) =
  def
  & style_cssClass .~ Class "elastic"
  & style_inline .~ (
    def
    & at "position" ?~ "relative"
    & at "left" ?~ (show x <> "px")
    & at "top" ?~ (show y <> "px")
  )
  where
    x = mouseX `div` 2
    y = mouseY `div` 2
