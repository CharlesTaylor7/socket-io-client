{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
module SocketIO
  ( Client
  , Url
  , Stream
  , Event
  , Error
  , connect
  , send
  )
  where

import Prelude hiding (until)
import Control.Monad (unless, forever)
import Control.Exception (Exception, throwIO)
import Control.Concurrent.MVar
import System.IO (Handle, hSetBinaryMode, hSetBuffering, hIsEOF, BufferMode(..))
import System.Process

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Aeson as Json

import Pipes
import qualified Pipes.Prelude as Pipes


type Url = String
type Stream a = Producer a IO ()
type Event = Either String Json.Value
type Error = BS.ByteString


data Client = Client
  { handle :: Handle
  , lock   :: MVar ()
  }


connect :: Url -> IO (Client, Stream Event, Stream Error)
connect server = do
  -- start the node process running the socket.io client
  (Just stdin, Just stdout, Just stderr, _) <-
    createProcess (proc "node" ["js/new-socket-io", server])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  client <- mkClient stdin
  let events = readLines stdout >-> Pipes.map toJsonValue
  let errors = readLines stderr

  pure $ (client, events, errors)


mkClient :: Handle -> IO Client
mkClient handle = do
  hSetBinaryMode handle True
  hSetBuffering handle LineBuffering

  lock <- newMVar ()
  pure $ Client { handle, lock }


send :: Client -> Json.Array -> IO ()
send (Client handle lock) args = do
  let bs = Json.encode args

  -- Lazy bytestring put is not threadsafe,
  -- so we wrap it in a write lock
  takeMVar lock
  BSL.hPut handle bs
  BSL.hPut handle "\n"
  putMVar lock ()


readLines :: Handle -> Producer BS.ByteString IO ()
readLines handle = do
  liftIO $ do
    hSetBinaryMode handle True
    hSetBuffering  handle LineBuffering

  until (isEOF handle) $ do
    value <- getLineBS handle
    yield value


until :: Monad m => m Bool -> m () -> m ()
until predicate action = do
  done <- predicate
  if done
  then pure ()
  else action >> until predicate action


isEOF :: MonadIO m => Handle -> m Bool
isEOF = liftIO . hIsEOF


getLineBS :: MonadIO m => Handle -> m BS.ByteString
getLineBS = liftIO . BS.hGetLine


toJsonValue :: BS.ByteString -> Either String Json.Value
toJsonValue = Json.eitherDecode' . BSL.fromStrict
