module Prelude (module X) where

import Extra as X

-- base
import Control.Exception as X (throwIO)
import Control.Monad.Fix as X (MonadFix(..))
import Control.Monad.IO.Class as X (MonadIO)

-- relude
import Relude as X hiding (uncons, (??), Alt, mapMaybe, id)

-- lens
import Control.Lens as X hiding (element)
import Control.Lens.Extras as X (is)
import Data.Text.Lens as X hiding (text)
