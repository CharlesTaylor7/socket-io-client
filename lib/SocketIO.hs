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
import Control.Concurrent (forkIO)
import System.Process (ProcessHandle, CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)
import Control.Exception (Exception, throwIO)
import System.IO (Handle, hSetBinaryMode, hSetBuffering, hIsEOF, BufferMode(..))
import System.IO (IOMode(..), openFile)
import System.Exit (ExitCode(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8

import Pipes (MonadIO, Consumer, Producer, Pipe, liftIO, (>->), yield, cat, await)
import qualified Pipes
import qualified Pipes.Prelude as Pipes

import Paths_socket_io_client (getDataFileName)


type Url = String
type Stream m = Producer BS.ByteString m ()
type SocketEmit m = Consumer BS.ByteString m ()


errorLogFileName :: FilePath
errorLogFileName = "socket-io-client-errors.log"


connect :: forall m. (MonadFail m, MonadIO m) => Url -> m (SocketEmit m, Stream m)
connect server = do
  nodeScriptName <- liftIO $ getDataFileName "js/new-socket-io.js"

  -- start the node process running the socket.io client
  (Just stdin, Just stdout, Just stderr, processHandle) <- liftIO $
    createProcess (proc "node" [nodeScriptName, server])
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
      appendToFile errorLogFileName

  let client = mkClient stdin
  let events = readLines stdout >-> waitForConnect

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
    BS.hPut handle line
    BS.hPut handle "\n"


waitForConnect :: MonadIO m => Pipe BS.ByteString BS.ByteString m ()
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
