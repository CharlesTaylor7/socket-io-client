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

type Dragging = All
pattern DragOn  = All True
pattern DragOff = All False

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


elastic
  :: forall t m a.
  (  Widget t m
  )
  => (Dynamic t Style -> m a)
  -> m a
elastic child = do
  rec
    (e, a) <- elStyle' "div" elasticStyle $ child dynChildStyle

    dragReferencePoint <- hold mempty mouseDown

    draggingBehavior <- hold DragOff toggleDragEvent

    dynChildStyle <-holdDyn def $ fmapCheap toStyle dragEvent

    let
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

      dragEvent :: Event t Point
      dragEvent = mouseMove
        & gate (coerceBehavior draggingBehavior)
        & attachWith minus dragReferencePoint


  pure a

toStyle :: Point -> Style
toStyle (coerce -> (mouseX, mouseY)) =
  def
  & style_cssClass .~ Class "elastic"
  & style_inline .~ (
    def
    & at "position" ?~ "relative"
    & at "left" ?~ (show x <> "px")
    & at "top" ?~ (show y <> "px")
  )
  where
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
