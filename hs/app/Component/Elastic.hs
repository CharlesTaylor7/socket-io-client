module Component.Elastic
  ( elastic
  ) where

import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

pattern DragOn  = All True
pattern DragOff = All False


elastic
  :: Widget t m
  => m a
  -> m a
elastic child = do
  (e, a) <- elClass' "div" "elastic" $ child

  let
    beginDragEvent = domEvent Mousedown e $> DragOn
    endDragEvent = domEvent Mouseup e $> DragOff


  dragBehavior <- hold DragOff (beginDragEvent <> endDragEvent)

  let
    -- scrollEvent = domEvent Scroll e
    moveEvent = domEvent Mousemove e
    dragEvent = gate (coerceBehavior dragBehavior) moveEvent

  -- performEvent $ scrollEvent <&> print
  performEvent $ moveEvent <&> (\(i, j) -> print ("move", i, j))
  performEvent $ dragEvent <&> (\(i, j) -> print ("drag", i, j))


  pure a
