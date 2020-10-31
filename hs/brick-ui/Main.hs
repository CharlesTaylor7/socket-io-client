import qualified GeneralsIO as G
import Generals.Types
import Node.FFI (loadReplay)
import Control.Lens.Unsafe

import qualified UI.Bot as Bot
import qualified UI.Replay as Replay

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS


data BotsEnv = BotsEnv
  { bots :: [G.Bot]
  }
  deriving stock (Generic)
  deriving anyclass (Json.FromJSON)


main :: IO ()
main = do
  gameServer <- G.newGame 2

  Right (bots :: BotsEnv) <- Json.eitherDecode' <$> BS.readFile "./bots.json"

  let bot = bots ^?! #bots . ix 1 $ "bot index"

  Bot.runUI bot gameServer
  pure ()

replayMain :: IO ()
replayMain = do
  replay <- loadReplay ReplayLocation
    { server = Server_Local
    , id = "rtQyMFIwv"
    }

  _ <- Replay.runUI replay
  pure ()
