import Node.FFI
import Types
import UI
import Generals.Replay.Simulate

import qualified Graphics.Vty as V

main :: IO ()
main = do
  replay <- download ReplayLocation
    { server = Server_Main
    , id = "rtQyMFIwv"
    }

  history <- toHistory replay
  brickMain history
