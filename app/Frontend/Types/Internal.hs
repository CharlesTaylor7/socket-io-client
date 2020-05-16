{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Frontend.Types.Internal where

import Relude(Map, Text)
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Reflex.Dom

import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend

import Common.Types

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

makeLenses ''Dimensions
makeLenses ''StyleInfo
makePrisms ''Pixels
makePrisms ''CSSClass
makePrisms ''DOMNode
