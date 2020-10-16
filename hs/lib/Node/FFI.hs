module Node.FFI where

import System.Process

import Generals.Replay.Types
import Generals.Replay.Decode


newtype Url = Url Text

replayUrl :: ReplayLocation -> Url
replayUrl replay
  = Url
  $ "https://generalsio-replays-"
  <> replay ^. #server . to urlSuffix
  <> ".s3.amazonaws.com/"
  <> replay ^. #id
  <> ".gior"


urlSuffix :: Server -> Text
urlSuffix Server_Main = "na"
urlSuffix Server_Bot = "bot"

download :: ReplayLocation -> IO Replay
download location = do
  let Url url = replayUrl location

  (exitCode, stdOut, stdErr) <-
    readProcessWithExitCode "node" ["js/download-replay.js", url ^. unpacked] ""

  putStrLn stdErr

  pure $ decode $ stdOut ^. packed
