{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Page.Replay.Orphans where

import Generals.Imports (Monoid(..), Default(..))

instance {-# overlaps #-} Monoid a => Default a where
  def = mempty
