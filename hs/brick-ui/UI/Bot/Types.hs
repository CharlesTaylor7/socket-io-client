module UI.Bot.Types where

import qualified Brick as Brick

import qualified GeneralsIO as G
import Generals.Types
import Control.Lens.Unsafe ((^?!))
import qualified Data.Aeson as Json


data Name
  -- views
  = GridView
  deriving (Eq, Ord, Show)

data AppEvent
  = GameEvent Json.Value
  | ErrorEvent Text
  deriving (Show)

type Widget = Brick.Widget Name


data AppState = AppState
  { events     :: !(Seq AppEvent)
  , turnIndex  :: !TurnIndex
  , bot        :: !G.Bot
  , gameConfig :: !G.GameConfig
  , client     :: !G.Client
  -- grid :: Grid
  }
  deriving stock (Generic, Show)
