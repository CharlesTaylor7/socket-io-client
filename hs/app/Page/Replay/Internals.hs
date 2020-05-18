{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Page.Replay.Internals where


import Generals.Imports (Widget, Event, newTriggerEvent, performEvent_, ffor)

import Page.Replay.Orphans
import Page.Replay.Types

import Js.Imports
import qualified Js.FFI as FFI


downloadReplay :: Widget t m => ReplayLocation -> m (Event t Replay)
downloadReplay location = do
  let url = toJSString $ replayUrl location

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
