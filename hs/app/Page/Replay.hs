{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Types
import Page.Replay.Download (download, toEvent)

import Component.Grid
import Prelude hiding ((#), (!), (!!))

import Language.Javascript.JSaddle
import qualified Js.FFI as FFI


replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do


  blank

test_download :: Widget t m => m ()
test_download = do
  (event, triggerEvent) <- newTriggerEvent
  let trigger = liftIO . triggerEvent
  srcDyn <- holdDyn (Url "") event

  iframe srcDyn
  trigger $ Url "https://generalsio-replays-na.s3.amazonaws.com/HOVnMO6cL.gior"
  blank


replayUploader :: Widget t m => m (Event t JSVal)
replayUploader = do
  (uploadEl, _) <- fileInputElement

  let inputEvent = domEvent Input uploadEl
  fileContentsEvent <- performEvent $
    inputEvent <&> \_ -> do
      print "input event fired!"
      file_contents <- liftIO $ do
        elVal <- toJSVal . _element_raw $ uploadEl
        files <- elVal ! ("files" :: Text)
        file <- files !! 0
        file # ("text" :: Text) $ ()

      toEvent $ FFI.Promise file_contents

  behavior <- hold never fileContentsEvent
  pure $ switch behavior
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
