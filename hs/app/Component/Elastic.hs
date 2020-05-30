module Component.Elastic
  ( elastic
  ) where

import Prelude hiding (subtract)
import Reflex hiding (elDynClass)
import Types

import Data.Group
import Data.These
import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Component.Elastic.Types

elastic
  :: forall t m a.
  (  Widget t m
  )
  => InitialDimensions
  -> (Dynamic t Style -> m a)
  -> m a
elastic dims child = do
  let
    elasticStyle :: Style
    elasticStyle = def
      & style_cssClass .~ Class "elastic"
      & style_inline .~ (
        def
        & at "width" ?~ "100vw"
        & at "height" ?~ "100vh"
      )
  rec
    (e, a) <- elStyle' "div" elasticStyle $ child dynChildStyle

    dragReferencePoint <- accumB add zero $
      alignEventWithMaybe combineMousedownAndMouseup mouseDown mouseUp

    draggingBehavior <- hold DragOff toggleDragEvent

    -- dragAmount :: Dynamic t (Point Int)
    dragAmount <- holdDyn zero $ dragEvent

    -- range from quarter size to 4 times as big
    zoomLevel <- accumDyn (\a b -> a + b & clamp (-750) 3000) 0 $ traceEventWith show wheelEvent

    let
      dynScale = zoomLevel <&> \level -> 1 + 0.001 * level

      dynChildStyle :: Dynamic t Style
      dynChildStyle = zipDynWith combine dragAmount dynScale

      toDoublePoint :: Point Int -> Point Double
      toDoublePoint = coerce . convert . coerce
        where
          toDouble :: Int -> Double
          toDouble = fromIntegral

          convert :: (Int, Int) -> (Double, Double)
          convert (a, b) = (toDouble a, toDouble b)

      combine :: Point Int -> Double -> Style
      combine offset zoom = toStyle dims $ Transform (toDoublePoint offset) zoom

      wheelEvent :: Event t Double
      wheelEvent = domEvent Wheel e & fmapCheap _wheelEventResult_deltaY

      dragEvent :: Event t (Point Int)
      dragEvent = mouseMove
        & gate (coerceBehavior draggingBehavior)
        & attachWith subtract dragReferencePoint

      mouseMove, mouseDown, mouseUp :: Event t (Point Int)
      mouseMove = mouseEvent Mousemove e
      mouseDown = mouseEvent Mousedown e
      mouseUp   = mouseEvent Mouseup   e

      mouseLeave :: Event t ()
      mouseLeave = domEvent Mouseleave e

      toggleDragEvent :: Event t Dragging
      toggleDragEvent = fold
        [ mouseDown  $> DragOn
        , mouseUp    $> DragOff
        , mouseLeave $> DragOff
        ]

  pure a

clamp :: Ord n => n -> n -> n -> n
clamp min max n
  | n < min = min
  | n > max = max
  | otherwise = n

combineMousedownAndMouseup :: Group g => These g g -> Maybe g
combineMousedownAndMouseup = \case
  This a -> Just a
  That b -> Just $ invert b
  These _ _ ->
    error "Mouse up & down at the same time!?"

mouseEvent
  ::
  (  Reflex t
  ,  HasDomEvent t target eventName
  ,  DomEventType target eventName ~ (Int, Int)
  )
  => EventName eventName
  -> target
  -> Event t (Point Int)
mouseEvent tag e = domEvent tag e & coerceEvent
