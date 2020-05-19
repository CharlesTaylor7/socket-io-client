{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Default.Orphans where

import Reflex (Reflex, Event, Dynamic, never, constDyn)

import Data.Default

instance Reflex t => Default (Event t a) where
  def = never

instance {-# overlappable #-} (Applicative m, Default a) => Default (m a) where
  def = pure def

instance {-# overlappable #-} Monoid a => Default a where
  def = mempty
