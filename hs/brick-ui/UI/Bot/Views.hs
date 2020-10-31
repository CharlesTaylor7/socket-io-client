module UI.Bot.Views
  ( drawUI
  )
  where

import Prelude hiding (Empty, on)
import Generals.Types
import Control.Lens.Unsafe

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick as Scroll (ViewportType(..))

import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Forms

import Brick.Grid (GridStyle(..), Padding(..))
import qualified Brick.Grid as Grid

import qualified Data.Text as T

import UI.Bot.Types
import UI.Bot.Attrs (ownerAttr, terrainAttr)

import qualified Data.IntSet as Set


drawUI :: AppState -> [Widget]
drawUI appState =
  [ center $ txt "hello world"
  ]
