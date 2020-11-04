{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
module GeneralsIO.Protocol.Events
  ( Event(..)
  ) where
-- import Pipes (Pipe)
import qualified GeneralsIO.Events as Evt
import GeneralsIO.Protocol.Phases


-- | Event GADT
-- Event triggers the current next change
data Event (current :: Phase) (next :: Phase) where
  QueueUpdate      :: Evt.QueueUpdate      -> Event current next
  ChatMessageE     :: ChatMessageEvent current next => Evt.ChatMessage      -> Event current next
  Notify           :: Evt.Notify           -> Event current next -- any
  PreGameStart     :: Evt.PreGameStart     -> Event current next -- ?
  GameUpdate       :: Evt.GameUpdate       -> Event (InGame g) (InGame g)
  ErrorSetUsername :: Evt.ErrorSetUsername -> Event current next -- ?


class ChatMessageEvent (current :: Phase) (next :: Phase)
instance ChatMessageEvent (InQueue g) (InQueue g)
instance ChatMessageEvent (InGame g)  (InGame g)
