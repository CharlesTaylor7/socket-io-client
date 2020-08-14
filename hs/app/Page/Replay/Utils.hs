module Page.Replay.Utils
  ( getSavedReplays
  ) where

import Reflex
import Data.Align (Semialign, alignWith)

import Js.Imports
import Js.Utils (promiseToEvent)
import qualified Js.FFI as FFI

import Page.Replay.Types (ReplayLocation(..), Server(..))
import Page.Replay.Decode (decode)

instance FromJSVal ReplayLocation where
  fromJSVal jsVal = do
    let obj = Object jsVal
    replayId <- unsafeGetProp "replayId" obj
      >>= fromJSVal
    server <- unsafeGetProp "server" obj
      >>= fromJSVal

    pure $ ReplayLocation <$> (server >>= parseServer) <*> replayId

parseServer :: Text -> Maybe Server
parseServer "na" = Just Server_Main
parseServer "bot" = Just Server_Bot
parseServer _ = Nothing

getCachedReplays :: Effects t m => m (Event t [ReplayLocation])
getCachedReplays =
  liftIO FFI.cachedReplays
  >>= promiseToEvent

getSavedReplays :: Effects t m => m (Event t [ReplayLocation])
getSavedReplays =
  liftIO (FFI.fetchBody "saved/replays.json")
  >>= promiseToEvent
  <&> fmap decode

getAllReplays :: Effects t m => m (Event t [ReplayLocation])
getAllReplays =
  liftA2 combine getCachedReplays getSavedReplays
  <&> fmap nubOrd
  where
    combine = alignWith alignByMappend

alignByMappend :: Semigroup a => These a a -> a
alignByMappend (This x) = x
alignByMappend (That y) = y
alignByMappend (These x y) = x <> y
