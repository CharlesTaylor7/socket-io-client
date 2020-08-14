module Js.Utils
  ( PromiseToEvent
  , promiseToEvent
  , promiseToEventVia
  ) where

import Reflex
import Js.Imports
import Js.Types (Promise)

import qualified Js.FFI as FFI


type PromiseToEvent t m =
  ( Reflex t
  , TriggerEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , MonadIO (PushM t)
  )

promiseToEventVia
  :: (PromiseToEvent t m, FromJSVal a)
  => (JSVal -> IO a)
  -> Promise a
  -> m (Event t a)
promiseToEventVia convert promise = do
  (event, trigger) <- newTriggerEvent

  -- callback triggers event
  jsCallback <- liftIO . asyncCallback1 $ trigger

  -- bind callback to promise
  liftIO $ promise `FFI.promise_then` jsCallback

  -- get the underlying value of the jsVal via performEvent & FromJSVal typeclass
  -- FromJSVal requires monadic context, hence why this isn't just fmap
  pure $ flip pushAlways event $
    \jsVal -> liftIO $ do
      -- release js callback
      releaseCallback jsCallback
      -- convert js reference to haskell data type
      convert jsVal

promiseToEvent
  :: (PromiseToEvent t m, FromJSVal a)
  => Promise a
  -> m (Event t a)
promiseToEvent = promiseToEventVia fromJSValUnchecked
