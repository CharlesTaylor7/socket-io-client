{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
module GeneralsIO.Protocol where
-- import Pipes (Pipe)
import GeneralsIO.Events (Event)
import qualified GeneralsIO.Commands as Cmd



data GameType
  = OneVsOne -- ^ one player against another
  | TwoVsTwo -- ^ two on a team against two
  | FFA      -- ^ public games up to 8 people
  | Custom   -- ^ aka private games up to 12 people


data Phase
  = Connected -- ^ connected to the server
  | InQueue   -- ^ in a queue
  | InGame    -- ^ in a game
  | GameOver  -- ^ game has ended


data Command (phase :: Phase) (gameType :: GameType) where
  SetUsername   :: Cmd.SetUsername   -> Command phase     gameType
  Play          :: Cmd.Play          -> Command Connected gameType
  Join1v1       :: Cmd.Join1v1       -> Command Connected gameType
  SetCustomTeam :: Cmd.SetCustomTeam -> Command InQueue   Custom
  JoinTeam      :: Cmd.JoinTeam      -> Command InQueue   TwoVsTwo
  LeaveTeam     :: Cmd.LeaveTeam     -> Command InQueue   TwoVsTwo
  Cancel        :: Cmd.Cancel        -> Command InQueue   gameType
  SetForceStart :: Cmd.SetForceStart -> Command InQueue   gameType
  Attack        :: Cmd.Attack        -> Command InGame    gameType
  ClearMoves    :: Cmd.ClearMoves    -> Command InGame    gameType
  PingTile      :: Cmd.PingTile      -> Command InGame    gameType
  ChatMessage   :: Cmd.ChatMessage   -> Command phase     gameType
  LeaveGame     :: Cmd.LeaveGame     -> Command phase     gameType
  StarsAndRank  :: Cmd.StarsAndRank  -> Command phase     gameType
