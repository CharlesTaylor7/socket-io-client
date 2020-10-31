{-# language NoImplicitPrelude #-}
{-# language PackageImports #-}
module Prelude (module X) where

-- base
import "base" Prelude as X (span)
import Control.Arrow as X ((&&&), (|||))
import Control.Exception as X (throwIO, try, evaluate)
import Control.Monad.Fix as X (MonadFix(..))
import Control.Monad.IO.Class as X (MonadIO(..))
import Control.Monad.ST as X (ST, runST)
import Control.Monad as X (forever, void)
import Control.Concurrent as X (threadDelay, forkIO)

import Data.List.NonEmpty as X (NonEmpty(..), groupWith)
import Data.Traversable as X (for)
import Data.Monoid as X (First(..), Last(..))
import Text.Printf as X
import System.IO as X (openFile)
import Data.Bits as X ((.|.))

-- relude
import Relude as X hiding (Alt, (??), uncons, mapMaybe, id, toList)

-- containers
import Data.IntSet as X (IntSet)
import Data.Containers.ListUtils as X (nubOrd)
import Data.Sequence as X (Seq)

-- vector
import Data.Vector as X (Vector)

-- data-default
import Data.Default as X

-- generic-monoid
import Data.Monoid.Generic as X (GenericSemigroup(..), GenericMonoid(..))

-- generic-lens
import Data.Generics.Labels ()

-- lens
import Control.Lens.At as X
import Control.Lens.Cons as X
import Control.Lens.Each as X
import Control.Lens.Extras as X (is)
import Control.Lens.Empty as X
import Control.Lens.Fold as X hiding ((^?!))
import Control.Lens.Getter as X
import Control.Lens.Indexed as X
import Control.Lens.Iso as X
import Control.Lens.Lens as X
import Control.Lens.Prism as X
import Control.Lens.Review as X hiding ((#))
import Control.Lens.Setter as X
import Control.Lens.Traversal as X hiding (element, singular, unsafePartsOf)
import Control.Lens.Tuple as X
import Control.Lens.Wrapped as X
import Control.Lens.TH as X (makeLenses, makeLensesFor, makePrisms)

import Control.Lens.Zoom as X
import Data.Text.Lens as X hiding (text)
import Data.IntSet.Lens as X
import Numeric.Lens as X
-- unused
-- import Control.Lens.Equality as X
-- import Control.Lens.Level as X
-- import Control.Lens.Plated as X
-- import Control.Lens.Reified as X
-- import Control.Lens.Type as X

-- custom
import Control.Lens.Safe as X
