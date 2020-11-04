{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
module GeneralsIO.Protocol.Events
  ( Event(..)
  ) where
-- import Pipes (Pipe)
import qualified GeneralsIO.Events as Evt
import GeneralsIO.Protocol.Phases as Phase


-- | Event GADT
-- Event triggers the current next change
data Event (current :: Phase) (next :: Phase) where
  QueueUpdate      :: QueueUpdateEvent current next => Evt.QueueUpdate      -> Event current     next
  ChatMessageE     :: ChatMessageEvent current next => Evt.ChatMessage      -> Event current     next
  Notify           ::                                  Evt.Notify           -> Event current     next -- any
  PreGameStart     ::                                  Evt.PreGameStart     -> Event current     next -- ?
  GameStart        ::                                  Evt.GameStart        -> Event (InQueue g) (InGame g)
  GameUpdate       ::                                  Evt.GameUpdate       -> Event (InGame g)  (InGame g)
  GameOver         ::                                  Evt.GameOver         -> Event (InGame g)  Phase.GameOver
  ErrorSetUsername ::                                  Evt.ErrorSetUsername -> Event current     next -- ?


class ChatMessageEvent (current :: Phase) (next :: Phase)
instance ChatMessageEvent (InQueue g) (InQueue g)
instance ChatMessageEvent (InGame g)  (InGame g)


class QueueUpdateEvent (current :: Phase) (next :: Phase)
instance QueueUpdateEvent Connected   (InQueue g)
instance QueueUpdateEvent (InQueue g) (InQueue g)
