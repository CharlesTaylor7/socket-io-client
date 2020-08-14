module Js.Imports
  ( module X
  , (#)
  ) where

import GHCJS.Foreign.Callback as X
import GHCJS.Types as X
import Language.Javascript.JSaddle as X hiding (JSException, (#))
import Language.Javascript.JSaddle.Types as X
import JavaScript.Array as X hiding ((!), create)

import qualified Language.Javascript.JSaddle as JSaddle

infixr 2 #

(#) :: (MakeObject this, MakeArgs args) => this -> Text -> args -> JSM JSVal
(#) = (JSaddle.#)

