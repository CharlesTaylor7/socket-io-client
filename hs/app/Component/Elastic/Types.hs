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

data Point a = Point !a !a
  deriving stock (Show, Functor, Foldable, Traversable)

instance Num n => Semigroup (Point n) where
  Point a b <> Point c d = Point (a + c) (b + d)

instance Num n => Monoid (Point n) where
  mempty = Point 0 0

instance Num n => Group (Point n) where
  invert (Point x y) = Point (-x) (-y)

instance Num n => Abelian (Point n)

add :: Abelian n => n -> n -> n
add = (<>)

minus :: Abelian n => n -> n -> n
a `minus` b = a <> invert b

subtract :: Abelian n => n -> n -> n
subtract b a = a <> invert b

zero :: Abelian n => n
zero = mempty

scale :: Num n => n -> Point n -> Point n
scale scalar (Point x y) = Point (scalar * x) (scalar * y)

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
toStyle (width, height) (Transform (Point x y) (coerce -> scale)) =
  def
  & style_inline .~ (
    def
    & at "position" ?~ "relative"
    & at "width"  ?~ toText (scale * width)
    & at "height" ?~ toText (scale * height)
    & at "left"   ?~ toText (coerce x :: Pixels)
    & at "top"    ?~ toText (coerce y :: Pixels)
  )
