module Js.Types where

import Data.Default


newtype Url = Url Text

instance Default Url where
  def = Url ""
