{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Element
  ( Element
  )
  where

import Reflex.Dom hiding (Element)
import qualified Reflex.Dom as Reflex


type Element t m = Reflex.Element EventResult (DomBuilderSpace m) t
