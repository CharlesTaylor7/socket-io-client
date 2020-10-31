{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
module SocketIO
  ( Client
  , Url
  , EventStream
  , connect
  , send
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
import Control.Exception (Exception, throwIO)


data Client = Client
  { handle :: Handle
  , lock   :: MVar ()
  }
  deriving (Generic)


type Url = String

-- lazy stream of socket events
-- attempting to read the whole list will block the current thread
-- so make sure to process events in a background thread
type EventStream = [Json.Value]


connect :: Url -> IO (Client, EventStream)
connect server = do
  -- start the node process running the socket.io client
  (Just stdin, Just stdout, _, _) <-
    createProcess (proc "node" ["js/new-socket-io", server])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  events <- toEventStream stdout

  -- set our input handle to binary mode
  hSetBinaryMode stdin True

  -- initialize lock
  lock <- newMVar ()

  pure $ (Client { handle = stdin, lock }, events)


send :: Client -> Json.Array -> IO ()
send socket payload = do
  let handle = socket ^. #handle
  let lock = socket ^. #lock
  let bs = Json.encode payload

  -- Lazy bytestring put is not threadsafe, so we wrap it in an mvar write lock
  takeMVar lock
  BS.hPut handle bs
  BS.hPut handle "\n"
  putMVar lock ()


toEventStream :: Handle -> IO EventStream
toEventStream handle = do
  hSetBinaryMode handle True
  bs <- BS.hGetContents handle
  bs
    & BS.split (fromIntegral $ fromEnum $ '\n')
    & traverse (throwLeft . Json.eitherDecode')

data ParseError = ParseError String
  deriving (Show)

instance Exception ParseError

throwLeft :: Either String a -> IO a
throwLeft (Right a) = pure a
throwLeft (Left str) = throwIO $ ParseError str
