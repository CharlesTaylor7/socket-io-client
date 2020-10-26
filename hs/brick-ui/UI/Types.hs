module UI.Types where

import Types
import qualified Brick as Brick
import Brick.Forms


type CustomEvent = ()

-- | Named resources
data Name = GridView | JumpToTurn
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget Name


data AppState = AppState
  { history :: History
  , turnIndex :: TurnIndex
  , replay :: Replay
  , jumpToTurn :: Form TurnIndex CustomEvent Name
  }
  deriving stock (Generic)
