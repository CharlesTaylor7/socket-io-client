
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Types where

import Obelisk.Route (Encoder, R, FullRoute, PageName)
import Obelisk.Route.TH (deriveRouteComponent)

import Common.Imports

newtype GameConfig = GameConfig [BotName]
  deriving (Show)

newtype BotName = BotName Text
  deriving (Show)

newtype Id = Id Int
  deriving (Show)

makePrisms ''Id
makePrisms ''BotName

-- routes

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Setup :: FrontendRoute ()
  FrontendRoute_NewGame :: FrontendRoute GameConfig
  FrontendRoute_Game :: FrontendRoute Id

deriving instance Show a => Show (FrontendRoute a)

concat <$> traverse deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

type ErrorMonad = Either Text

type AppRouteEncoder = Encoder ErrorMonad Identity (R ( FullRoute BackendRoute FrontendRoute)) PageName

type QueryParams = Map Text (Maybe Text)
type Path = [Text]
