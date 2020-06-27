{-# language NoImplicitPrelude #-}
module Control.Lens.Safe
  ( previewOrError
  , (^??)
  , singularOrError
  )
  where

import Relude
import Control.Monad.Except
import Control.Lens
import Control.Lens.Internal.Bazaar


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

type BazFunctor f a = BazaarT (->) f a a
type Input f s t a = Traversing (->) f s t a a
type Output f s t a = Over (->) f s t a a

 -- singular :: (HasCallStack, Conjoined p, Functor f) => Traversing p f s t a a -> Over p f s t a a
singular_
  :: (HasCallStack, Applicative f)
  => (Input f s t a)
  -> (Output f s t a)
singular_ = singular


singularOrError = undefined
