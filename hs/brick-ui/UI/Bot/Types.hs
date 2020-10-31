module UI.Bot.Types where

import qualified Brick as Brick

import GeneralsIO (Bot, GameServer)
import Generals.Types
import Control.Lens.Unsafe ((^?!))
import qualified Data.Aeson as Json


data Name
  -- views
  = GridView
  deriving (Eq, Ord, Show)

type SocketEvent = Json.Value

type Widget = Brick.Widget Name


data AppState = AppState
  { events     :: !(Seq SocketEvent)
  , turnIndex  :: !TurnIndex
  , bot        :: !Bot
  , gameServer :: !GameServer
  -- grid :: Grid
  }
  deriving stock (Generic)
