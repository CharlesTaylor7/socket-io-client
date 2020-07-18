module Page.Replay.Utils
  ( getCachedReplays
  ) where

import Reflex

import Js.Imports
import Js.Utils (promiseToEventVia)
import qualified Js.FFI as FFI

import Page.Replay.Types (ReplayLocation(..), Server(..))

instance FromJSVal ReplayLocation where
  fromJSVal jsVal = do
    let obj = Object jsVal
    replayId <- unsafeGetProp "replayId" obj
      >>= fromJSVal
    server <- unsafeGetProp "server" obj
      >>= fromJSVal
      -- <&> parseServer

    pure $ ReplayLocation <$> (server >>= parseServer) <*> replayId

parseServer :: Text -> Maybe Server
parseServer "na" = Just Server_Main
parseServer "bot" = Just Server_Bot
parseServer _ = Nothing


getCachedReplays :: Effects t m => m (Event t [ReplayLocation])
getCachedReplays = do
  cached <- liftIO FFI.cachedReplays
  promiseToEventVia fromJSValUncheckedListOf cached
