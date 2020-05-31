{-# language TemplateHaskell #-}
module Types where


newtype GameConfig = GameConfig [BotName]
  deriving (Show)

newtype BotName = BotName Text
  deriving (Show)

newtype Id = Id Int
  deriving (Show)


makePrisms ''Id
makePrisms ''BotName
makePrisms ''GameConfig
