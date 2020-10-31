{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module SocketIO
  ( SocketSend
  , SocketReceive
  , connect
  , send
  , receive
  )
  where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text)
import System.IO
import qualified Data.Aeson.Types as Json


newtype SocketSend = SocketSend Handle
newtype SocketReceive = SocketReceive Handle

newtype Url = Url Text

connect :: Url -> IO (SocketSend, SocketReceive)
connect (Url server) = do
  undefined

send :: SocketSend -> Json.Array -> IO ()
send = undefined

receive :: SocketReceive -> IO Json.Value
receive = undefined
