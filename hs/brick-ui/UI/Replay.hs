module UI.Replay (runUI) where

import Prelude hiding (Empty, on)
import Control.Lens.Unsafe
import Generals.Types
import Generals.Replay.Simulate (toHistory)

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
import UI.Replay.Forms (newJumpToTurnForm)


runUI :: Replay -> IO AppState
runUI replay = do
  history <- toHistory replay

  let turnIndex = TurnIndex 0
  let jumpToTurnForm = newJumpToTurnForm turnIndex
  defaultMain app $ AppState
    { history
    , turnIndex
    , replay
    , jumpToTurnForm
    }

app :: App AppState CustomEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const gridAttrMap
  }
