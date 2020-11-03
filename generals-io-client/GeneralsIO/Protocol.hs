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
  SetUsername :: Cmd.SetUsername -> Command phase gameType
