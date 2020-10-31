module GeneralsIO
  ( GameServer(..)
  , Bot(..)
  , newGame
  )
  where

data GameServer = GameServer
  { uuid       :: UUID
  , numPlayers :: Int
  }

data Bot = Bot
  { id :: Text
  , name :: Text
  }

type NumPlayers = Int

newGame :: NumPlayers -> IO GameServer
newGame numPlayers = do
  uuid <- nextRandom
  pure GameServer { uuid, numPlayers }
