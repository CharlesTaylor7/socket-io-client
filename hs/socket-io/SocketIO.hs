module SocketIO
  ( GameServer(..)
  , Bot(..)
  , InputH
  , OutputH
  , newGame
  , connect
  )
  where


data GameServer = GameServer {}
data Bot = Bot {}
type InputH = ()
type OutputH = ()

newGame :: IO GameServer
newGame = undefined

connect :: Bot -> GameServer -> IO (InputH, OutputH)
connect = undefined
