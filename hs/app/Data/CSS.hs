module Data.CSS where

import Data.CSS.Types
import Data.Default (Default(..))

-- instances
instance ToText Pixels where
  toText = view (_Pixels . re _Show . packed . to (<> "px"))

instance Default Style where
  def = Style mempty mempty

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

toAttrs :: Style -> Map Text Text
toAttrs style =
  let
    styleText =
      ifoldlOf (style_inline . ifolded) join "" style
    join key acc value =
      key <> ":" <> value <> ";" <> acc
    classText =
      style ^. style_cssClass . _Class
  in
    mempty
      & at "class" ?~ classText
      & at "style" ?~ styleText
