module UI.Bot.Types where

import qualified Brick as Brick

import GeneralsIO (Bot)
import Generals.Types
import Control.Lens.Unsafe ((^?!))
import qualified Data.Aeson as Json


-- | Named resources
data Name
  -- views
  = GridView
  -- form inputs
  | JumpToTurnInput
  deriving (Eq, Ord, Show)

type SocketEvent = Json.Value

type Widget = Brick.Widget Name


data AppState = AppState
  { events    :: !(Seq SocketEvent)
  , turnIndex :: !TurnIndex
  , bot       :: !Bot
  -- grid :: Grid
  }
  deriving stock (Generic)
