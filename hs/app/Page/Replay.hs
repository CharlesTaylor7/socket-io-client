module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Cache
import Page.Replay.Download
import Page.Replay.Types

import Component.Grid
import Component.FileUpload
import Component.FileDownload

import Prelude hiding ((#), (!), (!!))

import Js.Imports
import Js.Types
import Js.Utils
import qualified Js.FFI as FFI

import Data.Default
import Data.Default.Orphans

import Types (width, height)

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  replayEvent <- download
  widgetHold blank $ replayEvent <&> gameReplay
  blank

gameReplay :: Widget t m => Replay -> m ()
gameReplay replay = do
  (rawEvent, trigger) <- newTriggerEvent

  jsCallback <- liftIO $ asyncCallback1 trigger
  liftIO $ FFI.registerOnKeydown jsCallback

  keyEvent <- (performEvent $ rawEvent <&> toKey) <&> mapMaybe toCommand
  map <- toMap replay keyEvent
  void $ grid map

toKey :: MonadIO m => JSVal -> m KeyCode
toKey jsval = liftIO $ do
  keyCode <- jsval ! ("keyCode" :: Text)
  fromJSValUnchecked keyCode

toCommand :: KeyCode -> Maybe Command
toCommand code =
  case keyCodeLookup code of
    ArrowLeft  -> Just Backwards
    ArrowRight -> Just Forwards
    _          -> Nothing

download :: Widget t m => m (Event t Replay)
download = downloadReplay ReplayLocation
  { replay_id = "HOVnMO6cL"
  , server = Server_Main
  }

unsafeFromJust :: Maybe a -> a
unsafeFromJust (Just a) = a
unsafeFromJust Nothing  = error "<unsafeFromJust>"

toMap
  :: (Reflex t, MonadFix m, MonadHold t m, MonadIO m, PostBuild t m, DomBuilder t m)
  => Replay
  -> Event t Command
  -> m (Generals.Map t)
toMap replay@Replay{..} commandEvent = do
  let seed = newCache tiles

  dynGrid <- (fmap (unsafeFromJust . currentGrid)) <$> foldDyn (commandReducer replay) seed commandEvent

  pure $ Generals.Map
    { _dimensions = dimensions
    , _tiles = dynGrid
    }
  where
    toCoord = view $ coordinated (dimensions ^. width)

    tiles = mountainsMap <> citiesMap <> generalsMap <> clearMap

    mountainsMap = fromList $
      [ (toCoord index, Mountain)
      | index <- mountains
      ]
    citiesMap = fromList $
      [ (toCoord index, City (Neutral `Army` size))
      | (index, size) <- zip cities cityArmies
      ]
    generalsMap = fromList $
      [ (toCoord index, General $ (Player id) `Army` 0)
      | (index, id) <- zip generals [0..]
      ]
    clearMap = fromList $
      [((i, j), def)
      | i <- [1..dimensions ^. width]
      , j <- [1..dimensions ^. height]
      ]
