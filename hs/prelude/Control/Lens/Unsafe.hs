{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
module Control.Lens.Unsafe
  ( singular
  , (^?!)
  )
  where
import Relude
import Control.Monad.Except (MonadError(..))
import Control.Lens (Getting, Over, Traversing, toListOf, getting, preview)
import Control.Lens.Traversal (unsafePartsOf)
import Control.Lens.Internal.Bazaar
import Data.Profunctor.Rep (Corepresentable(..))
import Data.Profunctor.Sieve (cosieve)

import qualified Control.Lens as Unsafe

infixl 8 ^?!

type PreviewContext = Text

(^?!) :: HasCallStack => s -> Getting (First a) s a -> PreviewContext -> a
(structure ^?! optic) context =
  preview optic structure
  & maybe (error message) identity
  where
    message =
      "unsafePreview: expected atleast one target in context: "
      <> context

{-# INLINE (^?!)  #-}
{-# WARNING (^?!) "Partial function; may throw impure exception" #-}


-- unsafePreview optic = (Unsafe.^?! optic)
-- {-# INLINE unsafePreview  #-}
-- {-# WARNING unsafePreview "Partial function; may throw impure exception" #-}

type SingularContext = Text

singular
  :: (HasCallStack, Functor f, p ~ (->))
  => SingularContext
  -> Traversing p f s t a b
  -> Over p f s t a b
singular context optic =
  \a_to_fb s ->
    let
      bazaar = optic sell s
    in
      case wares bazaar of
        [ware] -> unsafeOuts bazaar . singleton <$> a_to_fb ware
        []     -> error $ "singular: no matches for context: " <> context
        ls     -> error $ "singular: " <> show (length ls) <> " matches for context: " <> context
{-# WARNING singular "Partial function; may throw impure exception" #-}

singleton :: a -> [a]
singleton = pure
{-# INLINE singleton #-}

sell ::  a -> BazaarT (->) g a t t
sell w = BazaarT ($ w)
{-# INLINE sell #-}

unsafeOuts :: BazaarT (->) g a b t -> [b] -> t
unsafeOuts (BazaarT f) =
  f (\_ -> state unsafeUncons :: State [b] b)
  & evalState
{-# INLINE unsafeOuts #-}

unsafeUncons :: [a] -> (a, [a])
unsafeUncons (x:xs) = (x, xs)
{-# INLINE unsafeUncons #-}

wares :: BazaarT (->) g a b t -> [a]
wares = toListOf $ getting bazaar
{-# INLINE wares #-}


