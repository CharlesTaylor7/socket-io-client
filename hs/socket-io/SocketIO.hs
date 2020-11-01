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
import Control.Monad (unless, forever)

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8)

import Pipes
import Control.Concurrent.MVar
import System.IO
import System.Process
import Control.Exception (Exception, throwIO, catch)


data Client = Client
  { handle :: Handle
  , lock   :: MVar ()
  }
  deriving (Generic, Show)

instance Show (MVar a) where
  show _ = "MVar"


type Url = String

-- lazy stream of socket events
type EventStream = Producer Json.Value IO ()
type ErrorStream = Producer Text IO ()


connect :: Url -> IO (Client, EventStream, ErrorStream)
connect server = do
  -- start the node process running the socket.io client
  (Just stdin, Just stdout, Just stderr, _) <-
    createProcess (proc "node" ["js/new-socket-io", server])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  client <- mkClient stdin
  let events = toEventStream stdout
  let errors = toErrorStream stderr

  pure $ (client, events, errors)


mkClient :: Handle -> IO Client
mkClient handle = do
  hSetBinaryMode stdin True
  hSetBuffering stdin LineBuffering

  lock <- newMVar ()
  pure $ Client { handle, lock }


send :: Client -> Json.Array -> IO ()
send socket payload = do
  let handle = socket ^. #handle
  let lock = socket ^. #lock
  let bs = Json.encode payload

  -- Lazy bytestring put is not threadsafe, so we wrap it in an mvar write lock
  takeMVar lock
  BSL.hPut handle bs
  BSL.hPut handle "\n"
  putMVar lock ()


toErrorStream :: Handle -> Producer Text IO ()
toErrorStream handle = do
  liftIO $ hSetBinaryMode handle True
  liftIO $ hSetBuffering handle LineBuffering

  forever $ do
    value <- liftIO $ BS.hGetLine handle
    yield $ decodeUtf8 value


toEventStream :: Handle -> EventStream
toEventStream handle = do
  liftIO $ hSetBinaryMode handle True
  liftIO $ hSetBuffering handle LineBuffering
  loop
  where
    loop = do
      eof <- liftIO $ hIsEOF handle
      unless eof $ do
        value <- liftIO $ BS.hGetLine handle >>= throwLeft . Json.eitherDecode' . BSL.fromStrict
        yield value
        loop

data ParseError = ParseError String
  deriving (Show)

instance Exception ParseError

throwLeft :: Either String a -> IO a
throwLeft (Right a) = pure a
throwLeft (Left str) = throwIO $ ParseError str
