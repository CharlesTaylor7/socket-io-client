{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
module SocketIO
  ( SocketIO
  , Url(..)
  , connect
  , send
  , receive
  )
  where

import GHC.Generics (Generic)
import Data.Word (Word8)
import Lens.Micro
import Data.Generics.Labels

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


type Url = String

connect :: Url -> IO SocketIO
connect server = do
  -- start the node process running the socket.io client
  (Just stdin, Just stdout, Just stderr, _) <-
    createProcess (proc "node" ["js/new-socket-io", server])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
  -- set our handles to binary mode
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBinaryMode stderr True

  -- initialize writelock
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


receive :: SocketIO -> IO [Either String Json.Value]
receive socket = do
  let handle = socket ^. #receiveHandle
  bs <- BS.hGetContents handle
  pure $
    bs
      & BS.split (fromIntegral $ fromEnum $ '\n')
      & map Json.eitherDecode'
