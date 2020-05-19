{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay.Download where

import Reflex

import Page.Replay.Types
import Page.Replay.Decode

import Js.Imports
import Js.Utils

import qualified Js.FFI as FFI


downloadReplay :: Widget t m => ReplayLocation -> m (Event t Replay)
downloadReplay location = do
  let url = replayUrl location
  promise <- liftIO $ FFI.downloadReplay url
  replayEvent <- promiseToEvent promise
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
