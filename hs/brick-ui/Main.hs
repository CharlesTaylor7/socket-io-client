import Node.FFI
import Types
import UI
import Generals.Replay.Simulate

import Brick.Main (defaultMain)


main :: IO ()
main = do
  replay <- loadReplay ReplayLocation
    { server = Server_Local
    , id = "rtQyMFIwv"
    }

  history <- toHistory replay
  _ <- defaultMain app (history, TurnIndex 0)
  pure ()
