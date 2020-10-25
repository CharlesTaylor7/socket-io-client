module Node.FFI (loadReplay) where

import Types
import Generals.Replay.Decode

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified LZString as LZ


newtype Url = Url Text

loadReplay :: ReplayLocation -> IO Replay
loadReplay location = do
  let Url url = replayUrl location
  case location ^. #server of
    Server_Local -> do
      handle <- openFile (url ^. unpacked) ReadMode
      contents <- Base64.encode <$> BS.hGetContents handle

      pure $ decode $ LZ.decompressBase64 contents

    _ -> undefined


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

