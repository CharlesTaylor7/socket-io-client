import Node.FFI (loadReplay)
import Types
import qualified UI.Replay as Replay



main :: IO ()
main = do
  pure ()

replayMain :: IO ()
replayMain = do
  replay <- loadReplay ReplayLocation
    { server = Server_Local
    , id = "rtQyMFIwv"
    }

  _ <- Replay.runUI replay
  pure ()
