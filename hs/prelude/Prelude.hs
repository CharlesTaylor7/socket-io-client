{-# language NoImplicitPrelude #-}
{-# language PackageImports #-}
module Prelude (module X) where

-- base
import "base" Prelude as X (span)
import Control.Exception as X (throwIO)
import Control.Monad.Fix as X (MonadFix(..))
import Control.Monad.IO.Class as X (MonadIO)

import Data.List.NonEmpty as X (NonEmpty(..), groupWith)

-- relude
import Relude as X hiding (Alt, (??), uncons, mapMaybe, id, toList)

-- lens
import Control.Lens.At as X
import Control.Lens.Cons as X
import Control.Lens.Each as X
import Control.Lens.Extras as X (is)
import Control.Lens.Fold as X
import Control.Lens.Getter as X
import Control.Lens.Indexed as X
import Control.Lens.Iso as X
import Control.Lens.Lens as X
import Control.Lens.Prism as X
import Control.Lens.Review as X hiding ((#))
import Control.Lens.Setter as X
import Control.Lens.Traversal as X hiding (element)
import Control.Lens.Tuple as X
import Control.Lens.Wrapped as X
import Data.Text.Lens as X hiding (text)
import Numeric.Lens as X
import Control.Lens.TH as X (makeLenses, makePrisms)
-- unused
-- import Control.Lens.Empty as X
-- import Control.Lens.Equality as X
-- import Control.Lens.Level as X
-- import Control.Lens.Plated as X
-- import Control.Lens.Reified as X
-- import Control.Lens.Type as X
-- import Control.Lens.Zoom as X
