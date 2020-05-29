{-# language TemplateHaskell #-}
module Types where


newtype GameConfig = GameConfig [BotName]
  deriving (Show)

newtype BotName = BotName Text
  deriving (Show)

newtype Id = Id Int
  deriving (Show)

data Dimensions = Dimensions
  { _dimensions_width  :: Int
  , _dimensions_height :: Int
  }
  deriving (Eq, Show)

makePrisms ''Id
makePrisms ''BotName
makePrisms ''GameConfig

makeLenses ''Dimensions
