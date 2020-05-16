{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}
{-# Language MultiParamTypeClasses #-}
{-# Language UndecidableSuperClasses #-}
module Frontend.Types.Constraints where

-- import Relude(Map, Text)
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Reflex.Dom
import Network.HTTP.Req (MonadHttp(..))

import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Frontend

import Common.Types

type SSR_Widget_Constraints js t m
  = ObeliskWidget js t (R FrontendRoute) m

type SSR_Widget js t m a
  =  SSR_Widget_Constraints js t m
  => RoutedT t (R FrontendRoute) m a

type JS_Widget js t m a
  = ( PrerenderClientConstraint js t m
    , Client m ~ m
    , Prerender js t m
    )
  => Client m a

type FrontendConstraints js t m =
  ( PrerenderClientConstraint js t m
  , Client m ~ m
  , Prerender js t m
  )

newtype FrontendWidget m a = FrontendWidget
  { runFrontendWidget :: forall js t. FrontendConstraints js t m => m a
  }
