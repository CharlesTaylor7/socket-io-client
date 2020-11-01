{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
module SocketIO
  ( Client
  , Url
  , EventStream
  , ErrorStream
  , connect
  , send
  )
  where

import Control.Monad (unless, forever)

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Pipes
import Control.Concurrent.MVar
import System.IO
import System.Process
import Control.Exception (Exception, throwIO)


data Client = Client
  { handle :: Handle
  , lock   :: MVar ()
  }

type Url = String

-- lazy stream of socket events
type EventStream = Producer Json.Object IO ()
type ErrorStream = Producer BS.ByteString IO ()


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
send (Client handle lock) payload = do
  let bs = Json.encode payload

  -- Lazy bytestring put is not threadsafe, so we wrap it in an mvar write lock
  takeMVar lock
  BSL.hPut handle bs
  BSL.hPut handle "\n"
  putMVar lock ()


toErrorStream :: Handle -> Producer BS.ByteString IO ()
toErrorStream handle = do
  liftIO $ hSetBinaryMode handle True
  liftIO $ hSetBuffering handle NoBuffering

  forever $ do
    value <- liftIO $ BS.hGetLine handle
    yield $ value


toEventStream :: Handle -> EventStream
toEventStream handle = do
  liftIO $ hSetBinaryMode handle True
  liftIO $ hSetBuffering handle NoBuffering
  loop
  where
    loop = do
      eof <- liftIO $ hIsEOF handle
      unless eof $ do
        value <- liftIO $ BS.hGetLine handle >>= toJsonValue
        yield value
        loop

    toJsonValue :: BS.ByteString -> IO Json.Object
    toJsonValue = throwLeft . Json.eitherDecode' . BSL.fromStrict

    throwLeft :: Either String a -> IO a
    throwLeft (Right a) = pure a
    throwLeft (Left str) = throwIO $ ParseError str

data ParseError = ParseError String
  deriving (Show)

instance Exception ParseError
