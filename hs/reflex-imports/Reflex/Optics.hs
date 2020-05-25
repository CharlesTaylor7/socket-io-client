{-# language TemplateHaskell #-}
module Reflex.Optics where

import Reflex.Dom (InputElement(..))
import Control.Lens.TH (makeLenses)

makeLenses ''InputElement
