{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Js.Generals where

import Generals.Imports
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import Language.Javascript.JSaddle hiding (eval)
-- import qualified Language.Javascript.JSaddle as JSaddle

import Data.String.Interpolate (i)
import Js.Utils


data Server
  = Server_Main
  | Server_Bot

data ReplayLocation = ReplayLocation
  { server :: Server
  , replay_id :: Text
  }

type Replay = Map Text Text

download :: MonadJSM m => m Replay
download = downloadReplay location
  where
    location = ReplayLocation
      { replay_id = "HOVnMO6cL"
      , server = Server_Main
      }

downloadReplay :: (MonadJSM m) => ReplayLocation -> m Replay
downloadReplay location = do
  let url = replayUrl location
  undefined


replayUrl :: ReplayLocation -> Text
replayUrl ReplayLocation{..} = [i|https://generalsio-replays-#{urlSuffix server}.s3.amazonaws.com/#{replay_id}.gior|]

urlSuffix :: Server -> Text
urlSuffix Server_Main = "na"
urlSuffix Server_Bot = "bot"
