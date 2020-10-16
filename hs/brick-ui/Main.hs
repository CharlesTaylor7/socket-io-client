import Node.FFI
import Generals.Replay.Types
import UI


main :: IO ()
main = do
  replay <- download ReplayLocation
    { server = Server_Main
    , id = "rtQyMFIwv"
    }

  print $ replay
  pure ()


brickMain :: IO ()
brickMain = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100_000

  game <- initGame
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) app game

  pure ()


