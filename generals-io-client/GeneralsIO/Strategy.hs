module GeneralsIO.Strategy where

import Pipes (Pipe)
import GeneralsIO.Events (Event)
import GeneralsIO.Commands (Command)


type Strategy m =
  Pipe Event cmd m ()


