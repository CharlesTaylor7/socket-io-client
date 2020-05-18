{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Widget
  ( Widget
  )
  where

import Reflex.Dom hiding (Widget)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)

type Widget t m
  =
  ( MonadIO m
  , MonadFix m
  , Reflex t
  , DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  )
