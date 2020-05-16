{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Frontend where

import Relude
import Reflex.Dom

import Obelisk.Frontend (Frontend(..), ObeliskWidget(..))
import Obelisk.Route (R)
import Obelisk.Route.Frontend (setRoute, RoutedT(..))
import Obelisk.Generated.Static (static)

import Common.Types

import Data.Dependent.Sum

import Data.Dom (title, loadCSS, loadJS)
import App (app)


type Route = R (FrontendRoute)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      title "Generals.io Bot"
      loadCSS $ static @"css/main.css"
      loadCSS $ static @"css/setup.css"
      loadCSS $ static @"css/game.css"
      loadJS $ static @"js/socket.io.slim.js"
      loadJS $ static @"js/lz-string.min.js"
  , _frontend_body = app

    -- prerender_ (el "div" $ text "hello") $ do


      -- routeEvent <- updated <$> askRoute
      -- performEvent routeEvent (_action)
  }
