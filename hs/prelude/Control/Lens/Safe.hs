{-# language NoImplicitPrelude #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language AllowAmbiguousTypes #-}

module Control.Lens.Safe
  ( previewOrError
  , (^??)
  , singularOrError
  )
  where

import Relude
import Control.Monad.Except (MonadError(..))
import Control.Lens
import Control.Lens.Traversal (singular, unsafePartsOf)
import Control.Lens.Internal.Bazaar
-- import Control.Lens.Internal.Context (sell)
import Data.Profunctor.Rep (Corepresentable(..))
import Data.Profunctor.Sieve (cosieve)

maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e Nothing  = throwError e
maybeToEither _ (Just a) = pure a
{-# INLINE maybeToEither #-}


previewOrError :: MonadError e m => Getting (First a) s a -> e -> s -> m a
previewOrError optic error structure =
  preview optic structure
  & maybeToEither error
{-# INLINE previewOrError #-}


infixl 8 ^??

(^??) :: MonadError e m => s -> Getting (First a) s a -> e -> m a
(structure ^?? optic) error = previewOrError optic error structure
{-# INLINE (^??) #-}

type Baz f a = BazaarT (->) f a a
type Input f s t a = (a -> Baz f a a) -> s -> Baz f a t
type Output f s t a = (a -> f a) -> s -> f t

 -- singular :: (HasCallStack, Conjoined p, Functor f) => Traversing p f s t a a -> Over p f s t a a
singular_
  :: (HasCallStack)
  => Traversal' s a
  -> Lens' s a
singular_ = singular

singularOrError
  :: (MonadError e m)
  => Traversal' s a
  -> e
  -> Lens' s a
singularOrError l e =
  \a_to_fb s ->
    let
      b = l sell s
    in
      case targets b of
        (w:ws) -> a_to_fb w <&> unsafeOuts b . (:ws)
         -- []  -> a_to_fb (error "")      <&> unsafeOuts b . singleton

singleton :: a -> [a]
singleton = pure

sell ::  a -> BazaarT (->) g a t t
sell w = BazaarT ($ w)

type BazT = BazaarT (->)

unsafeOuts :: BazaarT (->) g a b t -> [b] -> t
unsafeOuts (BazaarT f) =
  f (\_ -> state unsafeUncons :: State [b] b)
  & evalState
{-# INLINE unsafeOuts #-}

unsafeUncons :: [a] -> (a, [a])
unsafeUncons (x:xs) = (x, xs)
{-# INLINE unsafeUncons #-}

targets :: BazaarT (->) g a b t -> [a]
targets = toListOf $ getting bazaar
{-# INLINE targets #-}


