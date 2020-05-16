{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Js.SocketIO where

import qualified Data.Text as T
import Language.Javascript.JSaddle hiding (eval)
import qualified Language.Javascript.JSaddle as JSaddle
import Data.JSString

import Data.String.Interpolate (i)
import Data.String.Interpolate.Conversion (Interpolatable)

import Js.Utils

import Frontend.Imports hiding (Event, (#))

deriving instance MakeArgs JSString

newtype Socket = Socket Object
newtype Url = Url Text
newtype Event = Event JSString
  deriving (ToJSVal, ToJSString)

newtype Handler = Handler Function

connect :: MonadJSM m => Url -> m Socket
connect (Url server) = Socket . Object <$> eval script
  where
    script :: Text
    script =
      [i|(function() {
        var socket = io('#{server}');
        var onEvent = socket.onevent;
        socket.onevent = function (packet) {
          var eventName = packet.data[0];
          // Log packets that don't have event handlers
          if (!socket._callbacks.hasOwnProperty(`$${eventName}`)) {
            console.log('Unhandled packet:', JSON.stringify(packet))
          }
          onEvent.call(this, packet);
        }
        return socket;
      })()|]

changeHandler :: MonadJSM m => Socket -> Event -> Handler -> m ()
changeHandler (Socket socket) (Event event) (Handler handler) = liftJSM $ do
  socket # "removeListener" $ event
  socket # "on" $ (event, handler)
  pure ()

noOp :: MonadJSM m => m Handler
noOp = liftJSM $ Handler <$>
  (function $ (\_ _ _ -> pure ()))

ignoreEvent :: MonadJSM m => Socket -> Event -> m ()
ignoreEvent socket event = noOp >>= changeHandler socket event

setBasicHandlers :: MonadJSM m => Socket -> m ()
setBasicHandlers socket = do
  let toEvent = Event . JSString . T.pack
  traverse_ (ignoreEvent socket . toEvent) $
    [ "pre_game_start"
    , "notify"
    , "chat_message"
    , "error_set_username"
    ]
