{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.CSS where

import Data.CSS.Types
import Data.Default (Default(..))

-- instances
instance ToText Pixels where
  toText = view (_Pixels . re _Show . packed . to (<> "px"))

instance Default StyleInfo where
  def = StyleInfo
    { _inlineStyle = mempty
    , _cssClass = mempty
    }

instance Semigroup CSSClass where
  Class "" <> a = a
  a <> Class "" = a
  Class a <> Class b = Class $ a <> " " <> b

instance Monoid CSSClass where
  mempty = Class ""

number = Class "number"
primary = Class "primary"
secondary = Class "secondary"
selected = Class "selected"

toAttrs :: StyleInfo -> Map Text Text
toAttrs styleInfo =
  let
    styleText =
      ifoldlOf (inlineStyle . ifolded) join  "" styleInfo
    join key acc value =
      key <> ":" <> value <> ";" <> acc
    classText =
      styleInfo ^. cssClass . _Class
  in
    mempty
      & at "class" ?~ classText
      & at "style" ?~ styleText
