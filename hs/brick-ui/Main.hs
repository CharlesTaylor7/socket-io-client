import Node.FFI
import Generals.Replays.Types

main :: IO ()
main = do
  download ReplayLocation
    { server = Server_Main
    , id = "rtQyMFIwv"
    }

  pure ()
