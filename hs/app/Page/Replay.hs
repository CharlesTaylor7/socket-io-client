{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Types
import Page.Replay.Download
import Js.Utils

import Component.Grid
import Component.FileUpload
import Component.FileDownload

import Prelude hiding ((#), (!), (!!))

import Js.Imports
import Js.Types
import qualified Js.FFI as FFI

import Data.Default
import Data.Default.Orphans

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Types (width, height)

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

data Command
  = Backwards
  | Forwards
  deriving (Show)

data Cache = Cache
  { currentIndex :: Int
  , history :: Map Int Grid
  }

currentGrid :: Cache -> Grid
currentGrid Cache {..} = history ^?! ix currentIndex

toMap
  :: (Reflex t, MonadFix m, MonadHold t m)
  => Replay
  -> Event t Command
  -> m (Generals.Map t)
toMap Replay{..} commandEvent = do
  let seed = Cache { currentIndex = 0, history = fromList [(0, tiles)]}
  dynGrid <- fmap currentGrid <$> foldDyn reducer seed commandEvent

  pure $ Generals.Map
    { _dimensions = dimensions
    , _tiles = dynGrid
    }
  where
    reducer :: Command -> Cache -> Cache
    reducer command cache = cache

    toCoord index =
      let (j, i) = index `divMod` (dimensions ^. width)
      in (i+1, j+1)

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
