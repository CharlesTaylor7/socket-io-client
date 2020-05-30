module Component.Elastic
  ( elastic
  ) where

import Prelude hiding (subtract)
import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Data.Group

import Data.These

newtype Point = Point (Sum Int, Sum Int)
  deriving newtype (Semigroup, Monoid, Group, Abelian)

add :: Abelian n => n -> n -> n
add = (<>)

minus :: Abelian n => n -> n -> n
a `minus` b = a <> invert b

subtract :: Abelian n => n -> n -> n
subtract b a = a <> invert b

zero :: Abelian n => n
zero = mempty

newtype Dragging = Dragging Bool
  deriving Semigroup via All

{-# complete DragOn, DragOff #-}
pattern DragOn  = Dragging True
pattern DragOff = Dragging False


elastic
  :: forall t m a.
  (  Widget t m
  )
  => (Dynamic t Style -> m a)
  -> m a
elastic child = do
  rec
    (e, a) <- elStyle' "div" elasticStyle $ child dynChildStyle

    dragReferencePoint <- accumB add zero $
      alignEventWithMaybe combineMousedownAndMouseup mouseDown mouseUp

    draggingBehavior <- hold DragOff toggleDragEvent

    dragAmount <- holdDyn zero $ dragEvent
    zoomLevel <- accumDyn (+) 0 $ fmapCheap _wheelEventResult_deltaY wheelEvent

    let
      dynChildStyle :: Dynamic t Style
      dynChildStyle = zipDynWith toStyle dragAmount zoomLevel

      wheelEvent :: Event t WheelEventResult
      wheelEvent = traceEventWith show $ domEvent Wheel e

      dragEvent :: Event t Point
      dragEvent = mouseMove
        & gate (coerceBehavior draggingBehavior)
        & attachWith subtract dragReferencePoint

      mouseMove, mouseDown, mouseUp :: Event t Point
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

combineMousedownAndMouseup :: These Point Point -> Maybe Point
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
  -> Event t Point
mouseEvent tag e = domEvent tag e & coerceEvent


toStyle :: Point -> Double -> Style
toStyle (coerce -> (mouseX, mouseY)) zoomLevel =
  def
  & style_cssClass .~ Class "elastic"
  & style_inline .~ (
    def
    & at "position" ?~ "relative"
    & at "width" ?~ percentage
    & at "height" ?~ percentage
    & at "left" ?~ (show x <> "px")
    & at "top" ?~ (show y <> "px")
  )
  where
    percentage = show (50 + zoomLevel) <> "%"
    x = mouseX :: Int
    y = mouseY :: Int


elasticStyle :: Style
elasticStyle = def
  & style_cssClass .~ Class "elastic"
  & style_inline .~ (
    def
    & at "width" ?~ "100vw"
    & at "height" ?~ "100vh"
  )
