{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveGeneric #-}

module Frontend.Imports
  ( module Common.Imports
  , module Frontend.Imports.TH
  , module Control.Monad.Fix
  , module Control.Monad.IO.Class
  , module Reflex.Dom
  , module Reflex.Dom.Location
  , module Obelisk.Route
  , module Obelisk.Route.Frontend
  , module Obelisk.Frontend
  , module Data.Default
  , module Language.Javascript.JSaddle
  , module Network.URI
  ) where

import Common.Imports
import Frontend.Imports.TH

import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO)
import Reflex.Dom
import Reflex.Dom.Location

import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend

import Data.Default (Default(..))
import JSDOM.Types (SerializedScriptValue(..))

import GHCJS.Prim.Internal (JSVal(..), jsNull)
import Language.Javascript.JSaddle (MonadJSM(..))
import Network.URI

deriving instance Show HistoryCommand

deriving instance Generic HistoryStateUpdate
deriving instance Show HistoryStateUpdate

deriving instance Generic SerializedScriptValue
instance Show SerializedScriptValue where
  show _ = "<js val>"

instance Default HistoryStateUpdate
instance Default SerializedScriptValue

instance Default JSVal where
  def = jsNull

instance Default Text where
  def = ""
