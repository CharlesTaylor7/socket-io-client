{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}

module Page.Replay.Download where

import Reflex

import Page.Replay.Types
import Page.Replay.Decode

import Js.Imports
import qualified Js.FFI as FFI


-- toEvent :: FFI.Promise -> Spider_Widget (Event Spider JSVal)

type ToEvent_Constraints t m =
  ( Reflex t
  , Monad m
  , MonadIO (Performable m)
  , PerformEvent t m
  , MonadIO m
  , TriggerEvent t m
  )
promiseToEvent :: (ToEvent_Constraints t m) => FFI.Promise -> m (Event t JSVal)
promiseToEvent promise = do
  (event, trigger) <- newTriggerEvent

  -- callback triggers event
  jsCallback <- liftIO . asyncCallback1 $ trigger

  -- bind callback to promise
  liftIO $ FFI.promise_then promise jsCallback

  -- release js callback after download completes
  performEvent_ $
    event <&> \_ -> liftIO $
      releaseCallback jsCallback

  pure event


downloadReplay :: Widget t m => ReplayLocation -> m (Event t Replay)
downloadReplay location = do
  let url = replayUrl location

  (replayEvent, trigger) <- newTriggerEvent

  -- callback marshals jsval to text & triggers event
  jsCallback <- liftIO . asyncCallback1 $
    \jsVal -> do
      Just replayText <- fromJSVal jsVal
      trigger replayText

  -- execute download
  liftIO $ FFI.downloadReplay url jsCallback

  -- release js callback after download completes
  performEvent_ $
    replayEvent <&> \_ -> liftIO $
      releaseCallback jsCallback

  pure $ replayEvent <&> decode


download :: Widget t m => m (Event t Replay)
download = downloadReplay location
  where
    location = ReplayLocation
      { replay_id = "HOVnMO6cL"
      , server = Server_Main
      }

replayUrl :: ReplayLocation -> Url
replayUrl ReplayLocation{..}
  = Url $
     "https://generalsio-replays-"
  <> urlSuffix server
  <> ".s3.amazonaws.com/"
  <> replay_id
  <> ".gior"

urlSuffix :: Server -> Text
urlSuffix Server_Main = "na"
urlSuffix Server_Bot = "bot"
