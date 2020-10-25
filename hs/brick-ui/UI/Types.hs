module UI.Types where

import qualified Brick as Brick
-- Types

-- | Ticks mark passing of time
--
-- the app's custom event type
data Tick = Tick

-- | Named resources
data Name = GridView
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget Name
type AppState = (History, TurnIndex)
