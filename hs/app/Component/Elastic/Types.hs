module Component.Elastic.Types where

import Prelude hiding (subtract)
import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Data.Group

import Data.These

newtype Point n = Point (Sum n, Sum n)
  deriving newtype (Semigroup, Monoid, Group, Abelian)
  deriving stock (Show)

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

data Transform = Transform
  { _transform_offset :: Point Double
  , _transform_scale  :: Double
  }

type InitialDimensions = (Pixels, Pixels)

toStyle :: InitialDimensions -> Transform -> Style
toStyle (width, height) (Transform (coerce -> (x, y)) (coerce -> scale)) =
  def
  & style_inline .~ (
    def
    & at "position" ?~ "relative"
    & at "width"  ?~ toText (scale * width)
    & at "height" ?~ toText (scale * height)
    & at "left"   ?~ toText (x :: Pixels)
    & at "top"    ?~ toText (y :: Pixels)
  )
