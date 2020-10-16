import Node.FFI
import Generals.Replay.Types

main :: IO ()
main = do
  replay <- download ReplayLocation
    { server = Server_Main
    , id = "rtQyMFIwv"
    }

  print $ replay
  pure ()
