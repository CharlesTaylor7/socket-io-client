module Node.FFI where

import System.Process
import Data.Aeson (Array, FromJSON)
import Generals.Replays.Types

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
urlSuffix Server_Main = "na" ^. packed
urlSuffix Server_Bot = "bot" ^. packed

download :: ReplayLocation -> IO Replay
download location = do
  let Url url = replayUrl location
  print url

  (exitCode, stdOut, stdErr) <-
    readProcessWithExitCode "node" ["js/download-replay", url ^. unpacked] ""

  putStr stdOut
  putStr stdErr

  pure undefined
