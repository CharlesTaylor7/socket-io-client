{-# language NoImplicitPrelude #-}
module Prelude (module X) where

-- base
import Control.Exception as X (throwIO)
import Control.Monad.Fix as X (MonadFix(..))
import Control.Monad.IO.Class as X (MonadIO)

import Data.List.NonEmpty as X (NonEmpty(..), groupWith, scanl)

-- relude
import Relude as X hiding (Alt, (??), uncons, mapMaybe, id, scanl)

-- lens
import Control.Lens as X hiding (element)
import Control.Lens.Extras as X (is)
import Data.Text.Lens as X hiding (text)
import Numeric.Lens as X

-- zippers
import Control.Zipper as X

-- data-nat16
import Data.Nat16 as X
