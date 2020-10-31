module UI.Bot.Types where

import Generals.Types
import qualified Brick as Brick
import Brick.Forms
import Control.Lens.Unsafe ((^?!))

type CustomEvent = ()

-- | Named resources
data Name
  -- views
  = GridView
  -- form inputs
  | JumpToTurnInput
  deriving (Eq, Ord, Show)


type Widget = Brick.Widget Name


data AppState = AppState
  { history :: History
  , turnIndex :: TurnIndex
  , replay :: Bot
  , jumpToTurnForm :: Form TurnIndex CustomEvent Name
  }
  deriving stock (Generic)

currentGame :: AppState -> GameInfo
currentGame = do
  history <- view #history
  turn <- view $ #turnIndex . _TurnIndex
  pure $
    history ^?! ix turn $ "History turn index"
