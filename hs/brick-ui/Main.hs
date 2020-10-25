import Node.FFI
import Types
import UI
import UI.Types
import Generals.Replay.Simulate

import Brick.Main (defaultMain)


main :: IO ()
main = do
  replay <- loadReplay ReplayLocation
    { server = Server_Local
    , id = "rtQyMFIwv"
    }

  history <- toHistory replay
  _ <- defaultMain app $ AppState history (TurnIndex 0) replay
  pure ()
