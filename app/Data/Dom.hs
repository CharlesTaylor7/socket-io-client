{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Dom where

import Relude
import Reflex.Dom
import Frontend.Types
import Data.CSS (toAttrs)
import Control.Lens

button :: DomBuilder t m => Text -> Text -> m (Event t ())
button className display = do
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
        -> m (Element EventResult (DomBuilderSpace m) t, a)
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
            -> m (Element EventResult (DomBuilderSpace m) t, a)
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
