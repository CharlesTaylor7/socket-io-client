module Component.Elastic
  ( elastic
  ) where

import Prelude hiding (subtract)
import Reflex hiding (elDynClass)
import Types

import Data.Group
import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Component.Elastic.Types hiding (Point)
import qualified Component.Elastic.Types as E

type Point = E.Point Double
pattern Point x y = E.Point x y
{-# complete Point #-}

elastic
  :: forall t m a.
  (  Widget t m
  )
  => InitialDimensions
  -> ScaleRange
  -> (Dynamic t Style -> m a)
  -> m a
elastic dims scaleRange child = do
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
    dynChildStyle <- getTransform scaleRange e <&> fmap (toStyle dims)

  pure a


getTransform
  :: forall t m. Widget t m
  => ScaleRange
  -> Element t m
  -> m (Dynamic t Transform)
getTransform scaleRange e = do
  let
    wheelEvent :: Event t Double
    wheelEvent = domEvent Wheel e & fmapCheap _wheelEventResult_deltaY

    mouseMove, mouseDown, mouseUp :: Event t Point
    mouseMove = mouseEvent Mousemove e
    mouseDown = mouseEvent Mousedown e
    mouseUp   = mouseEvent Mouseup   e

  mousePosition :: Behavior t Point <-
    hold zero mouseMove

  let
    mouseLeave :: Event t Point
    mouseLeave = domEvent Mouseleave e & tag mousePosition

  dragging :: Behavior t Dragging <-
    hold DragOff $ fold
      [ mouseDown  $> DragOn
      , mouseUp    $> DragOff
      , mouseLeave $> DragOff
      ]

  dragReferencePoint :: Behavior t Point <-
    hold zero $ mouseDown

  let
    dragEnd :: Event t Point =
      [ mouseUp, mouseLeave ]
      & leftmost
      & gate (coerceBehavior dragging)
      & attachWith subtract dragReferencePoint

  dragDyn :: Dynamic t Point <-
    holdDyn zero $
      leftmost
        [ dragEnd    $> zero
        , mouseMove
          & gate (coerceBehavior dragging)
          & attachWith subtract dragReferencePoint
        ]

  zoomDyn :: Dynamic t Zoom <-
    wheelEvent
    & foldDyn (zoomReducer scaleRange) def

  objectPositionWithoutDrag :: Dynamic t Point <-
    accumDyn (&) zero $
      leftmost
        [ dragEnd <&> add
        , zoomDyn
          & updated
          & attachWith
            (\mouse zoom ->
              add mouse . scale (1 + zoom ^. zoom_delta) . subtract mouse
            )
           mousePosition
        ]

  let
    objectPosition :: Dynamic t Point
    objectPosition = zipDynWith add objectPositionWithoutDrag dragDyn

  pure $
    zipDynWith Transform objectPosition (zoomDyn <&> _zoom_scale)

zoomReducer :: ScaleRange -> Double -> Zoom -> Zoom
zoomReducer (lo, hi) =
  \delta zoom ->
    let
      sensitivity = 0.001
      rescaledDelta = delta * sensitivity
      total = zoom ^. zoom_scale
      newTotal = rescaledDelta + total & clamp lo hi
      observedDelta = newTotal - total
    in
      Zoom newTotal observedDelta

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
  -> Event t Point
mouseEvent tag e = domEvent tag e & coerceEvent & fmapCheap toDoublePoint & coerceEvent

toDoublePoint :: (Int, Int) -> Point
toDoublePoint (x, y) = Point (fromIntegral x) (fromIntegral y)
