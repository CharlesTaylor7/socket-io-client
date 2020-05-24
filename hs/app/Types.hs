{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
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

makePrisms ''Id
makePrisms ''BotName
makePrisms ''GameConfig

makeFieldsNoPrefix ''Dimensions
