{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
module Data.CSS.Types where

data StyleInfo = StyleInfo
  { _inlineStyle :: Map Text Text
  , _cssClass :: CSSClass
  }

newtype Pixels = Pixels Int
  deriving (Num)


newtype CSSClass = Class { unClass :: Text }
  deriving (Eq, Show)



makeFieldsNoPrefix ''StyleInfo
makePrisms ''Pixels
makePrisms ''CSSClass
