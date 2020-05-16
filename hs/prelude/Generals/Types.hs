{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generals.Types where

import Generals.Imports

type Widget m a
  = forall t.
  ( MonadIO m
  , MonadFix m
  , Reflex t
  , DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  )
  => m a

newtype GameConfig = GameConfig [BotName]
  deriving (Show)

newtype BotName = BotName Text
  deriving (Show)

newtype Id = Id Int
  deriving (Show)

data Dimensions = Dimensions
  { _height :: Int
  , _width :: Int
  }
  deriving (Eq, Show)

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
