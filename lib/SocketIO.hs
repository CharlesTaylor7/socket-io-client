{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import System.IO (Handle, hSetBinaryMode, hSetBuffering, hIsEOF, BufferMode(..))
import System.Process
import Control.Exception (Exception, throwIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Aeson as Json

import Pipes
import qualified Pipes.Prelude as Pipes

import System.IO (IOMode(..), openFile)
import System.Exit (ExitCode(..), exitWith)


type Url = String
type Stream = Producer BS.ByteString IO ()
type SocketEmit = Json.Array -> IO ()

data Client = Client
  { handle :: Handle
  , lock   :: MVar ()
  }


connect :: Url -> IO (SocketEmit, Stream)
connect server = do
  -- start the node process running the socket.io client
  (Just stdin, Just stdout, Just stderr, processHandle) <-
    createProcess (proc "node" ["js/new-socket-io", server])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  -- write errors to file
  _ <- forkIO $ do
    Pipes.runEffect $
      ( readLines stderr >>
        readExitCode processHandle >-> Pipes.map exitCodeString
      )
      >-> appendToFile "socket.io-client.error-log"


  client <- mkClient stdin
  events <- readLines stdout & waitForConnect

  pure $ (send client, events)


exitCodeString :: ExitCode -> BS.ByteString
exitCodeString code = Char8.pack $ "process exited with: " <> show code

readExitCode :: ProcessHandle -> Producer ExitCode IO ()
readExitCode handle = do
  exitCode <- liftIO $ waitForProcess handle
  yield exitCode


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


appendToFile :: FilePath -> Consumer BS.ByteString IO ()
appendToFile path = do
  handle <- liftIO $ openFile path AppendMode
  Pipes.for cat $ \line -> liftIO $ do
    BS.hPutStr handle line
    BS.hPutStr handle "\n"


waitForConnect :: MonadIO m => Producer BS.ByteString m () -> m (Producer BS.ByteString m ())
waitForConnect producer = do
  connect <- next producer
  case connect of
    Right ("connect", rest) -> pure rest
    Right (msg, _) -> liftIO $ throwIO $
      BadString msg
    Left _ -> liftIO $ throwIO $
      NoConnection

data InvalidConnection
  = BadString BS.ByteString
  | NoConnection

  deriving (Show)
instance Exception InvalidConnection

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
