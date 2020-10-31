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

import GHC.Generics (Generic)

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson.Types as Json


import System.IO
import System.Process

data SocketIO = SocketIO
  { sendHandle    :: Handle
  , receiveHandle :: Handle
  }
  deriving (Generic)


newtype Url = Url Text

connect :: Url -> IO SocketIO
connect (Url server) = do
  (Just stdin, Just stdout, Just stderr, _) <-
    createProcess (proc "node" ["js/new-socket-io", T.unpack server])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  pure $ SocketIO stdin stdout

send :: SocketIO -> Json.Array -> IO ()
send = undefined

receive :: SocketIO -> IO Json.Value
receive = undefined
