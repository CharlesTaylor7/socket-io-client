module UI.Replay where

import Prelude hiding (Empty, on)
import Control.Lens.Unsafe

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick as Scroll (ViewportType(..))

import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Forms

import Brick.Grid (GridStyle(..))
import qualified Brick.Grid as Grid

import qualified Data.Text as T

import UI.Replay.Types
import UI.Replay.Attrs (gridAttrMap)
import UI.Replay.Events (handleEvent)
import UI.Replay.Views (drawUI)

-- App definition
app :: App AppState CustomEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const gridAttrMap
  }
