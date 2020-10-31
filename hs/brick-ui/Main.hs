import GeneralsIO (Bot)
import Generals.Types
import Node.FFI (loadReplay)
import Control.Lens.Unsafe

import qualified UI.Bot as Bot
import qualified UI.Replay as Replay

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS


data BotsEnv = BotsEnv
  { bots :: [Bot]
  }
  deriving stock (Generic)
  deriving anyclass (Json.FromJSON)


main :: IO ()
main = do
  Right (bots :: BotsEnv) <- Json.eitherDecode' <$> BS.readFile "./bots.json"

  Bot.runUI $ (bots ^?! #bots . ix 0 $ "bot index")
  pure ()

replayMain :: IO ()
replayMain = do
  replay <- loadReplay ReplayLocation
    { server = Server_Local
    , id = "rtQyMFIwv"
    }

  _ <- Replay.runUI replay
  pure ()
