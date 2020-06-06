module Main (main) where

import Reflex
import Page.Replay
import Component.Elastic

import Data.Dom

main :: IO ()
main = mainWidget $ do
  replay
