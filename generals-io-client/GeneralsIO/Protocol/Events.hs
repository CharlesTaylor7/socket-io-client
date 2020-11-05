{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
module GeneralsIO.Protocol.Events
  ( Event(..)
  ) where
import qualified GeneralsIO.Events as Evt
import GeneralsIO.Protocol.Phases as Phase


-- | Event GADT
-- Event occurs during a particular phase
data Event (phase :: Phase) where
  QueueUpdate      :: QueueUpdateEvent current => Evt.QueueUpdate      -> Event current
  ChatMessageE     :: ChatMessageEvent current => Evt.ChatMessage      -> Event current
  Notify           ::                             Evt.Notify           -> Event current
  PreGameStart     ::                             Evt.PreGameStart     -> Event current
  GameStart        ::                             Evt.GameStart        -> Event (InQueue g) (InGame g)
  GameUpdate       ::                             Evt.GameUpdate       -> Event (InGame g)  (InGame g)
  GameOver         ::                             Evt.GameOver         -> Event (InGame g)  Phase.GameOver
  ErrorSetUsername ::                             Evt.ErrorSetUsername -> Event current


class ChatMessageEvent (phase :: Phase)
instance ChatMessageEvent (InQueue g)
instance ChatMessageEvent (InGame g)


class QueueUpdateEvent (phase :: Phase)
instance QueueUpdateEvent Connected
instance QueueUpdateEvent (InQueue g)
