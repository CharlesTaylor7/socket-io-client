{-# language TemplateHaskell #-}
module Data.CSS.Types where

data Style = Style
  { _style_inline :: Map Text Text
  , _style_cssClass :: CSSClass
  }

newtype Pixels = Pixels Double
  deriving (Num)


newtype CSSClass = Class { unClass :: Text }
  deriving (Eq, Show)


makeLenses ''Style
makePrisms ''Pixels
makePrisms ''CSSClass
