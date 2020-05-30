module Component.Elastic
  ( elastic
  ) where

import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


elastic
  :: Widget t m
  => m a
  -> m a
elastic child = do
  (e, a) <- elClass' "div" "elastic" $ child

  let
    scrollEvent = domEvent Scroll e
    dragEvent = domEvent Mousedown e

  performEvent $ scrollEvent <&> print
  performEvent $ dragEvent <&> print


  pure a
