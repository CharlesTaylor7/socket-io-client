module Component.FileUpload where

import Prelude hiding ((#))
import Reflex

import Js.Imports
import Js.Types
import Js.Utils
import qualified Js.FFI as FFI

import Data.Default


data UploadConfig = UploadConfig
  { accept :: Text
  , multiple :: Bool
  }

instance Default UploadConfig where
  def = UploadConfig
    { accept = ""
    , multiple = False
    }

fileUpload :: Widget t m => UploadConfig -> m (Event t JSVal)
fileUpload (UploadConfig{..}) = do
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
      <> "accept" =: accept
      & if multiple
        then includeMultipleAttr
        else identity
      )
      blank
    includeMultipleAttr = at "multiple" ?~ ""
