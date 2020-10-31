{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SocketIO
  ( SocketIO
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

import GHC.Generics (Generic)

data SocketIO = SocketIO
  { sendHandle    :: Handle
  , receiveHandle :: Handle
  }
  deriving (Generic)


newtype Url = Url Text

connect :: Url -> IO SocketIO
connect (Url server) = do
  undefined

send :: SocketIO -> Json.Array -> IO ()
send = undefined

receive :: SocketIO -> IO Json.Value
receive = undefined


