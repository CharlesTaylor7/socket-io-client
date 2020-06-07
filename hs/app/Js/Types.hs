module Js.Types
  ( Url(..)
  , Promise
  ) where

import Data.Default

import Js.Imports

newtype Url = Url Text

instance Default Url where
  def = Url ""


newtype Promise a = Promise JSVal
