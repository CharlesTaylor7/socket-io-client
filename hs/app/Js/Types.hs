module Js.Types
  ( Url(..)
  , Promise
  ) where

import Js.Imports

newtype Url = Url JSString

instance Default Url where
  def = Url ""


newtype Promise a = Promise JSVal
