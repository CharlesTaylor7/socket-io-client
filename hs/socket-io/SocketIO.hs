{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
module SocketIO
  ( SocketIO
  , connect
  , send
  , receive
  )
  where

import GHC.Generics (Generic)

import Lens.Micro
import Data.Generics.Labels
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS

import Control.Concurrent.MVar
import System.IO
import System.Process


data SocketIO = SocketIO
  { sendHandle    :: Handle
  , receiveHandle :: Handle
  , writeLock     :: MVar ()
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
  writeLock <- newMVar ()
  pure $ SocketIO stdin stdout writeLock

send :: SocketIO -> Json.Array -> IO ()
send socket payload = do
  let handle = socket ^. #sendHandle
  let lock = socket ^. #writeLock
  let bs = Json.encode payload

  -- Lazy bytestring put is not threadsafe, so we wrap it in an mvar write lock
  takeMVar lock
  BS.hPut handle bs
  BS.hPut handle "\n"
  putMVar lock ()


receive :: SocketIO -> IO Json.Value
receive = undefined
