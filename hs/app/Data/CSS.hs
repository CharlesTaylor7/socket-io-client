module Data.CSS where

import Data.CSS.Types


-- instances
instance ToText Pixels where
  toText = view (_Pixels . re _Show . packed . to (<> "px"))

instance Default Style where
  def = mempty

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
      style ^. style_class . _Class

    id =
      style ^. style_id . _Wrapped
  in
    mempty
      & at "class" ?~ classText
      & at "id" .~ id
      & at "style" ?~ styleText
