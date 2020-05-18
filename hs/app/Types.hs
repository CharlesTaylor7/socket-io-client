{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where


newtype GameConfig = GameConfig [BotName]
  deriving (Show)

newtype BotName = BotName Text
  deriving (Show)

newtype Id = Id Int
  deriving (Show)

data Dimensions = Dimensions
  { _width  :: Int
  , _height :: Int
  }
  deriving (Eq, Show)

instance Default Dimensions where
  def = Dimensions {}

data StyleInfo = StyleInfo
  { _inlineStyle :: Map Text Text
  , _cssClass :: CSSClass
  }

newtype Pixels = Pixels Int
  deriving (Num)

newtype CSSClass = Class { unClass :: Text }
  deriving (Eq, Show)

newtype DOMNode = Node { unNode :: Text }
  deriving (Eq, Show)


makePrisms ''Id
makePrisms ''BotName
makePrisms ''GameConfig

makeLenses ''Dimensions
makeLenses ''StyleInfo
makePrisms ''Pixels
makePrisms ''CSSClass
makePrisms ''DOMNode