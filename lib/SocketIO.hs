{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SocketIO
  ( Url
  , Stream
  , SocketEmit
  , connect
  )
  where

import Prelude hiding (until)
import Data.Function ((&))
import Control.Concurrent (forkIO)
import System.IO (Handle, hSetBinaryMode, hSetBuffering, hIsEOF, BufferMode(..))
import System.Process
import Control.Exception (Exception, throwIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8

import Pipes
import qualified Pipes.Prelude as Pipes

import System.IO (IOMode(..), openFile)
import System.Exit (ExitCode(..))


type Url = String
type Stream m = Producer BS.ByteString m ()
type SocketEmit m = Consumer BS.ByteString m ()


connect :: forall m. (MonadFail m, MonadIO m) => Url -> m (SocketEmit m, Stream m)
connect server = do
  -- start the node process running the socket.io client
  (Just stdin, Just stdout, Just stderr, processHandle) <- liftIO $
    createProcess (proc "node" ["js/new-socket-io", server])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  -- write errors to file
  _ <- liftIO $ forkIO $ do
    Pipes.runEffect $
      ( readLines stderr >>
        readExitCode processHandle >-> Pipes.map exitCodeString
      ) >->
      appendToFile "socket.io-client.error-log"

  let client = mkClient stdin
  let print = liftIO . Prelude.print
  let events = readLines stdout >-> Pipes.chain print >-> waitForConnect

  pure $ (client, events)


exitCodeString :: ExitCode -> BS.ByteString
exitCodeString (ExitFailure 125) = "Server disconnected"
exitCodeString code = Char8.pack $ "process exited with: " <> show code


readExitCode :: MonadIO m => ProcessHandle -> Producer ExitCode m ()
readExitCode handle = do
  exitCode <- liftIO $ waitForProcess handle
  yield exitCode


setBinaryLineBuffering :: MonadIO m => Handle -> m ()
setBinaryLineBuffering handle =
  liftIO $ do
    hSetBinaryMode handle True
    hSetBuffering  handle LineBuffering


mkClient :: MonadIO m => Handle -> Consumer BS.ByteString m ()
mkClient handle = do
  setBinaryLineBuffering handle

  Pipes.for cat $ \bs -> liftIO $ do
    -- write bytes to handle
    BS.hPut handle bs
    BS.hPut handle "\n"


appendToFile :: MonadIO m => FilePath -> Consumer BS.ByteString m ()
appendToFile path = do
  handle <- liftIO $ openFile path AppendMode
  Pipes.for cat $ \line -> liftIO $ do
    BS.hPutStr handle line
    BS.hPutStr handle "\n"


waitForConnect :: MonadIO m => Pipe BS.ByteString BS.ByteString m r
waitForConnect = do
  connect <- await
  case connect of
    "connect" -> cat
    msg -> liftIO $ throwIO $
      BadString msg

data InvalidConnection
  = BadString BS.ByteString
  | NoConnection

  deriving (Show)
instance Exception InvalidConnection

readLines :: MonadIO m => Handle -> Producer BS.ByteString m ()
readLines handle = do
  setBinaryLineBuffering handle
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
