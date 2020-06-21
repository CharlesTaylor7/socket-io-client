{-# language NoMonomorphismRestriction #-}
module Control.Lens.Unsafe
  ( singular
  , unsafePreview
  , (^?!)
  )
  where

import qualified Control.Lens as Unsafe


singular = Unsafe.singular
{-# INLINE singular #-}
{-# WARNING singular "Partial function; may throw impure exception" #-}


infixl 8 ^?!

(^?!) = (Unsafe.^?!)
{-# INLINE (^?!)  #-}
{-# WARNING (^?!) "Partial function; may throw impure exception" #-}


unsafePreview optic = (Unsafe.^?! optic)
{-# INLINE unsafePreview  #-}
{-# WARNING unsafePreview "Partial function; may throw impure exception" #-}

