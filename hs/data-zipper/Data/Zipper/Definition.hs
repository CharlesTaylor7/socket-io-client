{-# language NoImplicitPrelude #-}
{-# language StrictData #-}
{-# language DeriveGeneric #-}
module Data.Zipper.Definition where

import Relude

data Zipper a = Zipper
  { _prev  :: [a]
  , _focus :: a
  , _next  :: [a]
  }
  deriving (Eq, Show, Generic)
