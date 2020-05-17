module Js.Imports
  ( eval
  , eval_
  , module Relude
  , module Language.Javascript.JSaddle
  ) where

import Relude
import Language.Javascript.JSaddle hiding (eval)
import qualified Language.Javascript.JSaddle as JSaddle

eval :: MonadJSM m => Text -> m JSVal
eval script = liftJSM $ JSaddle.eval script

eval_ :: MonadJSM m => Text -> m ()
eval_ = void . eval
