{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
module Data.Zipper.TH where

import Control.Lens.TH (makeLenses)

import Data.Zipper.Definition


makeLenses ''Zipper
