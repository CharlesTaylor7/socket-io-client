module Js.Utils where

import Reflex
import Js.Imports

import qualified Js.FFI as FFI

type ToEvent_Constraints t m =
  ( Reflex t
  , PerformEvent t m
  , TriggerEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  )
promiseToEvent :: (ToEvent_Constraints t m, FromJSVal a) => FFI.Promise a -> m (Event t a)
promiseToEvent promise = do
  (event, trigger) <- newTriggerEvent

  -- callback triggers event
  jsCallback <- liftIO . asyncCallback1 $ trigger

  -- bind callback to promise
  liftIO $ promise `FFI.promise_then` jsCallback

  -- get the underlying value of the jsVal via performEvent & FromJSVal typeclass
  -- FromJSVall requires monadic context, hence why this isn't just fmap
  doneEvent <- performEvent $
    event <&> \jsVal -> liftIO $ do
      -- release js callback
      releaseCallback jsCallback
      -- convert js reference to haskell data type
      fromJSValUnchecked jsVal

  pure doneEvent
