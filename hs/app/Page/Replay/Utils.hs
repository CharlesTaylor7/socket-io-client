module Page.Replay.Utils
  ( getCachedReplays
  ) where

import Js.Imports
import qualified Js.FFI as FFI

import Page.Replay.Types (ReplayLocation(..), Server(..))


getCachedReplays :: IO [ReplayLocation]
getCachedReplays = do
  cachedReplays <- FFI.cachedReplays
  for (toList cachedReplays) $ \jsVal -> do
    let obj = Object jsVal
    replayId <- unsafeGetProp "replayId" obj
      >>= fromJSValUnchecked
    server <- unsafeGetProp "server" obj
      >>= fromJSValUnchecked
      <&> parseServer

    pure $ ReplayLocation server replayId



parseServer :: Text -> Server
parseServer "na" = Server_Main
parseServer "bot" = Server_Bot
