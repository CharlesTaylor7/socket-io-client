{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
module GeneralsIO.Protocol.Commands
-- import Pipes (Pipe)
import GeneralsIO.Events as Evt
import qualified GeneralsIO.Commands as Cmd


data Command (phase :: Phase)  where
  SetUsername   ::   {- ?? -}                   Cmd.SetUsername   -> Command phase
  StarsAndRank  ::   {- ?? -}                   Cmd.StarsAndRank  -> Command phase
  Play          ::                              Cmd.Play          -> Command Connected
  Join1v1       ::                              Cmd.Join1v1       -> Command Connected
  SetCustomTeam ::                              Cmd.SetCustomTeam -> Command (InQueue   Custom)
  JoinTeam      ::                              Cmd.JoinTeam      -> Command (InQueue   TwoVsTwo)
  LeaveTeam     ::                              Cmd.LeaveTeam     -> Command (InQueue   TwoVsTwo)
  Cancel        ::                              Cmd.Cancel        -> Command (InQueue   gameType)
  SetForceStart :: CanSetForceStart gameType => Cmd.SetForceStart -> Command (InQueue   gameType)
  Attack        ::                              Cmd.Attack        -> Command (InGame    gameType)
  ClearMoves    ::                              Cmd.ClearMoves    -> Command (InGame    gameType)
  PingTile      :: IsTeamGame gameType       => Cmd.PingTile      -> Command (InGame    gameType)
  ChatMessage   :: CanChat phase             => Cmd.ChatMessage   -> Command phase
  LeaveGame     :: CanLeaveGame phase        => Cmd.LeaveGame     -> Command phase

class CanLeaveGame (phase :: Phase)
instance CanLeaveGame (InGame g)
instance CanLeaveGame GameOver

class IsTeamGame (gameType :: GameType)
instance IsTeamGame TwoVsTwo
instance IsTeamGame Custom
instance IsTeamGame FFA

class CanSetForceStart (gameType :: GameType)
instance CanSetForceStart Custom
instance CanSetForceStart FFA

class CanChat (phase :: Phase)
instance CanChat (InQueue g)
instance CanChat (InGame g)
instance CanChat GameOver
