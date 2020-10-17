module Node.FFI (loadReplay) where

import System.Process

import Types
import Generals.Replay.Decode


newtype Url = Url Text


replayDir :: Server -> Text
replayDir Server_Local = "./replays/"
replayDir server =
  "https://generalsio-replays-"
  <>  ( case server of
          Server_Main -> "na"
          Server_Bot -> "bot"
      )
  <> ".s3.amazonaws.com/"


replayFile :: Text -> Text
replayFile id = id <> ".gior"

replayUrl :: ReplayLocation -> Url
replayUrl replay = Url
  $  replay ^. #server . to replayDir
  <> replay ^. #id . to replayFile


loadReplay :: ReplayLocation -> IO Replay
loadReplay location = do
  let Url url = replayUrl location

  (exitCode, stdOut, stdErr) <-
    readProcessWithExitCode "node" ["js/download-replay.js", url ^. unpacked] ""

  putStrLn stdErr

  pure $ decode $ stdOut ^. packed
