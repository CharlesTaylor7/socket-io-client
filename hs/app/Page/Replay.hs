{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Types
import Page.Replay.Download (download)
import Js.Utils

import Component.Grid
import Prelude hiding ((#), (!), (!!))

import Language.Javascript.JSaddle
import qualified Js.FFI as FFI

import Data.Default
import Data.Default.Orphans

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  iframe def
  replayUploader
  blank


test_download :: Widget t m => m ()
test_download = do
  urlDyn <- onceDyn $ Url "https://generalsio-replays-na.s3.amazonaws.com/HOVnMO6cL.gior"
  iframe urlDyn

replayUploader :: Widget t m => m (Event t JSVal)
replayUploader = do
  (uploadEl, _) <- fileInputElement

  fileContentsEvent <- performEvent $
    domEvent Input uploadEl <&> \_ -> liftIO $ do
      print "input event fired!"
      elVal <- toJSVal . _element_raw $ uploadEl
      files <- elVal ! ("files" :: Text)
      file <- files !! 0
      contents <- (file # ("text" :: Text) $ ())
      pure $ FFI.Promise contents

  switchWidgetEvent $
      promiseToEvent <$> fileContentsEvent

  where
    fileInputElement = elAttr'
      "input"
      (  "type" =: "file"
      <> "multiple" =: ""
      <> "accept" =: ".gior"
      )
      blank

iframe :: Widget t m => Dynamic t Url -> m ()
iframe srcDyn = elDynAttr
    "iframe"
    (srcDyn <&> \(Url src) -> baseAttrs & at "src" ?~ src)
    blank
    where
      baseAttrs = "style" =: "display:none"
