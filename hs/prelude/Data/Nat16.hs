{-# language NoImplicitPrelude #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
module Data.Nat16 (Nat16) where

import Relude

import Data.Default


newtype Nat16 = Nat16 Word16
  deriving newtype (Eq, Ord, Enum, Bounded, Show, Read, Integral, Real, Default)

maxWord16 :: Integer
maxWord16 = fromIntegral (maxBound :: Word16)

makeNat16 :: Integer -> Nat16
makeNat16 n
  | n < 0         = error $ "Nat16: can't be negative"
  | n > maxWord16 = error $ "Nat16: can't be greater than " <> show maxWord16
  | otherwise     = Nat16 $ fromIntegral n


instance Num Nat16 where
  fromInteger = makeNat16
  abs = identity
  signum = coerce . signum
  negate = error "Nat16: negate"
  a + b =
    let
      sum = Nat16 $ coerce a + coerce b
    in
      if sum < a
      then error "Nat16: overflow"
      else sum
  a - b =
    let
      subtracted = Nat16 $ coerce a - coerce b
    in
      if subtracted < 0
      then error "Nat16: underflow"
      else subtracted
  a * b =
    let
      product = Nat16 $ coerce a * coerce b
    in
      if product < a && product < b
      then error "Nat16: overflow"
      else product
