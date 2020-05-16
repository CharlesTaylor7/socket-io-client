{-# Language TemplateHaskell #-}
module Frontend.Imports.TH where

import Control.Lens (makeLenses, makePrisms)
import Reflex.Dom.Location

makePrisms ''HistoryCommand
makeLenses ''HistoryStateUpdate
