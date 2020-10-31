{-# LANGUAGE NamedFieldPuns #-}
module SocketIO
  ( GameServer(..)
  , Bot(..)
  , InputH
  , OutputH
  , newGame
  , connect
  )
  where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)


data GameServer = GameServer
  { uuid :: UUID
  }

data Bot = Bot {}
type InputH = ()
type OutputH = ()

newGame :: IO GameServer
newGame = do
  uuid <- nextRandom
  pure GameServer { uuid }

connect :: Bot -> GameServer -> IO (InputH, OutputH)
connect = undefined


{--
 const [gameId, botIndex] = process.argv.slice(2)
 const { bots } = require('../bots.json')
 const bot = bots[botIndex]
   socket.emit('set_username', bot.id, bot.name);
   socket.emit('join_private', gameId, bot.id);
  --}

