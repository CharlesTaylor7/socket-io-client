{-# language NoImplicitPrelude #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module Control.Lens.Safe
  ( previewOrError
  , (^??)
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

