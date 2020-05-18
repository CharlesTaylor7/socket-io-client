module Main (main) where

import Reflex
import Page.Replay

main :: IO ()
main = mainWidget $ do
  replay
