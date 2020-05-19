{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Types
import Page.Replay.Download (download)

import Component.Grid
import Prelude hiding ((!), (!!))

import Language.Javascript.JSaddle

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do

  uploadEl <- fileUploader
  let inputEvent = domEvent Input uploadEl
  performEvent_ $
    inputEvent <&> \_ -> liftIO $ do
      print "input event fired!"
      elVal <- toJSVal . _element_raw $ uploadEl
      stringifiedEl <- fromJSVal elVal
      print $ (stringifiedEl :: Maybe JSString)

      files <- elVal ! ("files" :: Text)
      file <- files !! 0
      file_hs <- fromJSVal file

      print $ (file_hs :: Maybe Text)
  blank

test_download :: Widget t m => m ()
test_download = do
  (event, triggerEvent) <- newTriggerEvent
  let trigger = liftIO . triggerEvent
  srcDyn <- holdDyn (Url "") event

  iframe srcDyn
  trigger $ Url "https://generalsio-replays-na.s3.amazonaws.com/HOVnMO6cL.gior"
  blank


fileUploader :: Widget t m => m (Element t m)
fileUploader = do
  (el, _) <- elAttr' "input" ("type" =: "file" <> "multiple" =: "") blank
  pure el


iframe :: Widget t m => Dynamic t Url -> m ()
iframe srcDyn = elDynAttr
    "iframe"
    (srcDyn <&> \(Url src) -> baseAttrs & at "src" ?~ src)
    blank
    where
      baseAttrs = "style" =:   "display:none"
