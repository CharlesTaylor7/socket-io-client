import Node.FFI
import Types
import UI
import UI.Types
import UI.Forms
import Generals.Replay.Simulate

import Brick.Main (defaultMain)


main :: IO ()
main = do
  replay <- loadReplay ReplayLocation
    { server = Server_Local
    , id = "rtQyMFIwv"
    }

  history <- toHistory replay

  let turnIndex = TurnIndex 0
  let jumpToTurnForm = newJumpToTurnForm turnIndex
  _ <- defaultMain app $ AppState
    { history
    , turnIndex
    , replay
    , jumpToTurnForm
    }
  pure ()
