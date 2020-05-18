{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Page.Replay.Orphans where

import Prelude (Monoid(..), Default(..))
--  (Monoid(..), Default(..))

instance {-# overlaps #-} Monoid a => Default a where
  def = mempty
