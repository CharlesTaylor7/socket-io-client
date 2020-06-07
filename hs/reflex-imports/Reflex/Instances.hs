module Reflex.Instances where

import Reflex.Dom (Event, Reflex, never)
import Data.Default

instance Reflex t => Default (Event t m) where
  def = never
