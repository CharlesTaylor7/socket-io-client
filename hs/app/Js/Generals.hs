{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Js.Generals where

import Generals.Imports (Event, newTriggerEventWithOnComplete)
import Reflex.Widget
import Js.Imports
import qualified Js.FFI as FFI


data Server
  = Server_Main
  | Server_Bot

data ReplayLocation = ReplayLocation
  { server :: Server
  , replay_id :: Text
  }

-- type Replay = Map Text Text
type Replay =  Text

download :: Widget t m => m (Event t Replay)
download = downloadReplay location
  where
    location = ReplayLocation
      { replay_id = "HOVnMO6cL"
      , server = Server_Main
      }

downloadReplay :: Widget t m => ReplayLocation -> m (Event t Replay)
downloadReplay location = liftJSM $ do
  let url = replayUrl location
  pure never


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
