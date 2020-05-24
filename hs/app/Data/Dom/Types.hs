{-# LANGUAGE TemplateHaskell #-}
{-# language FlexibleInstances #-}


module Data.Dom.Types where

newtype DOMNode = Node { unNode :: Text }
  deriving (Eq, Show)

makePrisms ''DOMNode
