module Prelude (module X) where

-- base
import Control.Exception as X (throwIO)
import Control.Monad.Fix as X (MonadFix(..))
import Control.Monad.IO.Class as X (MonadIO)

import Data.List.NonEmpty as X (NonEmpty(..), groupWith)

-- relude
import Relude as X hiding (uncons, (??), Alt, mapMaybe, id)

-- lens
import Control.Lens as X hiding (element)
import Control.Lens.Extras as X (is)
import Data.Text.Lens as X hiding (text)
import Numeric.Lens as X
