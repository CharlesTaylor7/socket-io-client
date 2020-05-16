{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Js.Utils where

import Relude
import qualified Data.Text as T
import Language.Javascript.JSaddle hiding (eval)
import qualified Language.Javascript.JSaddle as JSaddle

import Data.String.Interpolate (i)

eval :: MonadJSM m => Text -> m JSVal
eval script = liftJSM $ JSaddle.eval script

eval_ :: MonadJSM m => Text -> m ()
eval_ = void . eval

console_log :: (MonadJSM m) => Text -> m ()
console_log text = eval_ [i|console.log('#{text}')|]

window_confirm :: (MonadJSM m) => Text -> Function -> m ()
window_confirm text f = eval_ [i|window.alert('#{text}')|]
