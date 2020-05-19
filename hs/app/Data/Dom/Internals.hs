{-# LANGUAGE ViewPatterns #-}
module Data.Dom.Internals where

import Reflex hiding (button)

import Data.CSS
import Data.CSS.Types
import Data.Dom.Types


button :: DomBuilder t m => CSSClass -> Text -> m (Event t ())
button (Class className) display = do
  (e, _) <- elClass' "button" className $ text display
  pure $ domEvent Click e


div :: DOMNode
div = Node "div"

elStyle :: forall t m a. DomBuilder t m
        => DOMNode
        -> StyleInfo
        -> m a
        -> m a
elStyle (Node name) (toAttrs -> attrs) =
  elAttr name attrs

elStyle' :: forall t m a . DomBuilder t m
        => DOMNode
        -> StyleInfo
        -> m a
        -> m (Element t m, a)
elStyle' (Node name) (toAttrs -> attrs) =
  elAttr' name attrs

elDynStyle :: forall t m a . (DomBuilder t m, PostBuild t m)
           => DOMNode
           -> Dynamic t StyleInfo
           -> m a
           -> m a
elDynStyle (Node name) (fmap toAttrs -> dynAttrs) =
  elDynAttr name dynAttrs

elDynStyle' :: forall t m a . (DomBuilder t m, PostBuild t m)
            => DOMNode
            -> Dynamic t StyleInfo
            -> m a
            -> m (Element t m, a)
elDynStyle' (Node name) (fmap toAttrs -> dynAttrs) =
  elDynAttr' name dynAttrs

title :: DomBuilder t m => Text -> m ()
title = el "title" . text

loadCSS :: DomBuilder t m => Text -> m ()
loadCSS file = elAttr "link" attrs blank
  where attrs = mempty
          & at "href" ?~ file
          & at "type" ?~ "text/css"
          & at "rel" ?~ "stylesheet"

loadJS :: DomBuilder t m => Text -> m ()
loadJS file = elAttr "script" attrs blank
  where attrs = "src" =: file
