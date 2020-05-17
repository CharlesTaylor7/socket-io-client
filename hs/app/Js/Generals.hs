{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Js.Generals where

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
type Replay = Text

download :: MonadJSM m => m Replay
download = downloadReplay location
  where
    location = ReplayLocation
      { replay_id = "HOVnMO6cL"
      , server = Server_Main
      }

downloadReplay :: (MonadJSM m) => ReplayLocation -> m Replay
downloadReplay location = liftJSM $ do
  let url = replayUrl location
  let jsVal = FFI.downloadReplay (toJSString url)
  replayText <- fromJSValUnchecked jsVal
  pure replayText


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
