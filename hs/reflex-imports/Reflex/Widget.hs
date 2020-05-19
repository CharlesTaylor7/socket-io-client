{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Widget
  ( Widget
  )
  where

import Reflex.Dom hiding (Widget)
import qualified Reflex.Dom as Reflex

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)

import qualified GHCJS.DOM.Types as DOM

type Spider_Widget a = Reflex.Widget a

type Base t m
  =
  ( MonadIO m
  , MonadFix m
  , DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadFix (Performable m)
  , RawElement (DomBuilderSpace m) ~ DOM.Element
  )

type Widget t m
  =
  ( Reflex t
  , Base t m
  , Base t (Performable m)
  , Performable (Performable m) ~ Performable m
  )
