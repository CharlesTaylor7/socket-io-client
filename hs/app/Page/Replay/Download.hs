module Page.Replay.Download
  ( downloadReplay
  ) where

import Reflex

import Page.Replay.Types
import Page.Replay.Decode

import Js.Imports
import Js.Utils

import Js.Types


import qualified Js.FFI as FFI


downloadReplay
  :: PromiseToEvent t m
  => ReplayLocation
  -> m (Event t Replay)
downloadReplay location = do
  let url = replayUrl location
  promise <- liftIO $ FFI.downloadReplay url
  replayEvent <- promiseToEvent promise
  pure $ replayEvent <&> decode


replayUrl :: ReplayLocation -> Url
replayUrl replay
  = Url $
     "https://generalsio-replays-"
  <> replay ^. replayLocation_server . to urlSuffix
  <> ".s3.amazonaws.com/"
  <> replay ^. replayLocation_id
  <> ".gior"


urlSuffix :: Server -> Text
urlSuffix Server_Main = "na"
urlSuffix Server_Bot = "bot"
