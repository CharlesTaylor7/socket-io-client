{-# language TemplateHaskell #-}
module Data.CSS.Types where


data Style = Style
  { _style_inline :: Map Text Text
  , _style_class :: CSSClass
  , _style_id :: Last Text
  }
  deriving stock (Eq, Generic)
  deriving Semigroup via GenericSemigroup Style
  deriving Monoid    via GenericMonoid Style

instance Semigroup CSSClass where
  Class "" <> a = a
  a <> Class "" = a
  Class a <> Class b = Class $ a <> " " <> b

instance Monoid CSSClass where
  mempty = Class ""


newtype Pixels = Pixels Double
  deriving (Num)


newtype CSSClass = Class { unClass :: Text }
  deriving (Eq, Show)


makeLenses ''Style
makePrisms ''Pixels
makePrisms ''CSSClass
