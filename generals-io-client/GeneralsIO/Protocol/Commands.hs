{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
module GeneralsIO.Protocol.Commands
  ( Command(..)
  ) where

import qualified GeneralsIO.Commands as Cmd
import GeneralsIO.Protocol.Phases


data Command (phase :: Phase)  where
  SetUsername   ::                              Cmd.SetUsername   -> Command phase
  StarsAndRank  ::                              Cmd.StarsAndRank  -> Command phase
  Play          ::                              Cmd.Play          -> Command Connected
  Join1v1       ::                              Cmd.Join1v1       -> Command Connected
  Join2v2       ::                              Cmd.Join1v1       -> Command Connected
  JoinTeam      ::                              Cmd.JoinTeam      -> Command (InQueue   TwoVsTwo)
  LeaveTeam     ::                              Cmd.LeaveTeam     -> Command (InQueue   TwoVsTwo)
  JoinPrivate   ::                              Cmd.JoinPrivate   -> Command Connected
  SetCustomTeam ::                              Cmd.SetCustomTeam -> Command (InQueue   Custom)
  Cancel        ::                              Cmd.Cancel        -> Command (InQueue   gameType)
  SetForceStart :: CanSetForceStart gameType => Cmd.SetForceStart -> Command (InQueue   gameType)
  Attack        ::                              Cmd.Attack        -> Command (InGame    gameType)
  ClearMoves    ::                              Cmd.ClearMoves    -> Command (InGame    gameType)
  PingTile      :: CanPingTile gameType       => Cmd.PingTile     -> Command (InGame    gameType)
  ChatMessageC  :: CanChat phase             => Cmd.Message       -> Command phase
  LeaveGame     :: CanLeaveGame phase        => Cmd.LeaveGame     -> Command phase

class CanLeaveGame (phase :: Phase)
instance CanLeaveGame (InGame g)
instance CanLeaveGame GameOver

class CanPingTile (gameType :: GameType)
instance CanPingTile TwoVsTwo
instance CanPingTile Custom
instance CanPingTile FFA

class CanSetForceStart (gameType :: GameType)
instance CanSetForceStart Custom
instance CanSetForceStart FFA

class CanChat (phase :: Phase)
instance CanChat (InQueue g)
instance CanChat (InGame g)
instance CanChat GameOver
