{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
module GeneralsIO.Protocol.Phases where
-- import Pipes (Pipe)
import GeneralsIO.Events as Evt
import qualified GeneralsIO.Commands as Cmd


data GameType
  = OneVsOne -- ^ one player against another
  | TwoVsTwo -- ^ two on a team against two
  | FFA      -- ^ public games up to 8 people
  | Custom   -- ^ aka private games up to 12 people


data Phase
  = Connected        -- ^ connected to the server
  | InQueue GameType -- ^ in a queue
  | InGame  GameType -- ^ in a game
  | GameOver         -- ^ game has ended

