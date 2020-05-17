{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module Js.Generals where

import Generals.Imports (Widget, Event, newTriggerEventWithOnComplete)

import Js.Imports
import qualified Js.FFI as FFI

-- type Replay = Map Text Text
type Replay =  Text

downloadReplay :: Widget t m => ReplayLocation -> m (Event t Replay)
downloadReplay location = do
  let url = toJSString $ replayUrl location
  (replayEvent, trigger) <- newTriggerEventWithOnComplete

  rec
    jsCallback <- liftIO $ asyncCallback1 onDownload
    let onDownload jsVal = do
          Just replayText <- fromJSVal jsVal
          trigger replayText $ releaseCallback jsCallback

  liftIO $ FFI.downloadReplay url jsCallback

  pure replayEvent


data Server
  = Server_Main
  | Server_Bot

data ReplayLocation = ReplayLocation
  { server :: Server
  , replay_id :: Text
  }



download :: Widget t m => m (Event t Replay)
download = downloadReplay location
  where
    location = ReplayLocation
      { replay_id = "HOVnMO6cL"
      , server = Server_Main
      }



replayUrl :: ReplayLocation -> Text
replayUrl ReplayLocation{..}
  =  "https://generalsio-replays-"
  <> urlSuffix server
  <> ".s3.amazonaws.com/"
  <> replay_id
  <> ".gior"

urlSuffix :: Server -> Text
urlSuffix Server_Main = "na"
urlSuffix Server_Bot = "bot"
