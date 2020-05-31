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
    dynChildStyle <- getTransform e <&> fmap (toStyle dims)

  pure a

eitherOr :: Show a => These a a -> Maybe a
eitherOr (This a) = Just a
eitherOr (That a) = Just a
eitherOr (These a b) = Nothing & trace ("Coincidence: " <> show (a, b))

foldAlign :: (Reflex t, Show a, Foldable f) => f (Event t a) -> Event t a
foldAlign = foldl' (alignEventWithMaybe eitherOr) never

getTransform :: forall t m. Widget t m => Element t m -> m (Dynamic t Transform)
getTransform e = do
  let
    toggleDragEvent :: Event t Dragging
    toggleDragEvent = fold
      [ mouseDown  $> DragOn
      , mouseUp    $> DragOff
      , mouseLeave $> DragOff
      ]
  dragging :: Behavior t Dragging <- hold DragOff toggleDragEvent

  mousePositionDyn <- holdDyn zero mouseMove
  -- temporary
  let mousePosition = current mousePositionDyn

  dragReferencePointDyn :: Dynamic t (Point Int) <-
    accumDyn add zero $
    -- leftmost $
    foldAlign
      [ mouseDown
      , mouseUp & fmapCheap invert
      , mouseLeave
        & gate (coerceBehavior dragging)
        & attachWith (const . invert) mousePosition
      ]
  -- temporary
  let dragReferencePoint = current dragReferencePointDyn


  let
    dragEvent :: Event t (Point Int)
    dragEvent = mouseMove
      & gate (coerceBehavior dragging)
      & attachWith subtract dragReferencePoint


  dragAmount :: Dynamic t (Point Int) <- holdDyn zero $ dragEvent
-- range from quarter size to 4 times as big
  zoomLevel :: Dynamic t Double <- accumDyn (\a b -> a + b & clamp (-750) 3000) 0 $ wheelEvent

  let
    dynScale = zoomLevel <&> \level -> 1 + 0.001 * level

    dynTransform :: Dynamic t Transform
    dynTransform = zipDynWith combine dragAmount dynScale

    toDoublePoint :: Point Int -> Point Double
    toDoublePoint = view $ coerced . converted . coerced
      where
        converted = alongside (to int2Double) (to int2Double)
        int2Double :: Int -> Double
        int2Double = fromIntegral

    combine :: Point Int -> Double -> Transform
    combine offset zoom = Transform (toDoublePoint offset) zoom

  pure dynTransform
  where
    wheelEvent :: Event t Double
    wheelEvent = domEvent Wheel e & fmapCheap _wheelEventResult_deltaY

    mouseMove, mouseDown, mouseUp :: Event t (Point Int)
    mouseMove = mouseEvent Mousemove e
    mouseDown = mouseEvent Mousedown e
    mouseUp   = mouseEvent Mouseup   e

    mouseLeave :: Event t ()
    mouseLeave = domEvent Mouseleave e


clamp :: Ord n => n -> n -> n -> n
clamp min max n
  | n < min = min
  | n > max = max
  | otherwise = n

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
