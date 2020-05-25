{-# language NoImplicitPrelude #-}
module Data.Zipper.Operations where

import Control.Lens

import Data.Zipper.Definition
import Data.Zipper.TH

-- zip forward
-- if you fall off the front of the zipper, create a new focus by applying the handler to the previous focus
zipForwardWith :: (a -> a) -> Zipper a -> Zipper a
zipForwardWith f zipper =
  let
    currentFocus = zipper ^. focus
    (newFocus, newNext) =
      case zipper^.next of
        n : ns -> (n, ns)
        _      -> (f currentFocus, [])
  in
    zipper
    & prev %~ cons currentFocus
    & focus .~ newFocus
    & next .~ newNext

-- zip backward
-- if you fall off the back of the zipper, create a new focus by applying the handler to the previous focus
zipBackwardWith :: (a -> a) -> Zipper a -> Zipper a
zipBackwardWith f zipper =
  let
    currentFocus = zipper ^. focus
    (newFocus, newPrev) =
      case zipper^.prev of
        p : ps -> (p, ps)
        _      -> (f currentFocus, [])
  in
    zipper
    & next %~ cons currentFocus
    & focus .~ newFocus
    & prev .~ newPrev
