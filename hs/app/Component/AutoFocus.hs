module Component.AutoFocus
  ( autoFocus
  ) where

import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Js.Imports

pattern DragOn  = All True
pattern DragOff = All False


autoFocus
  :: forall t m a. Widget t m
  => m (Element t m, a)
  -> m a
autoFocus child = do
  (e, a) <- elClass "div" "auto-focus" $ child
  let
    focusEvent = domEvent Focus e

  performEvent (focusEvent <&> \_ -> print "focused!")

  liftIO $ do
    elJsVal <- toJSVal $ _element_raw e
    elJsVal # ("focus" :: Text) $ ()
    print "focus!"

  pure a
