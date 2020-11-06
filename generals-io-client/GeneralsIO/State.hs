{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module GeneralsIO.State
  ( GameState(..)
  , GridIndex
  , PlayerId
  , MonadState(..)
  , applyGameUpdate
  , mkGameState
  , module X
  )
  where

import GHC.Generics (Generic)
import Control.Monad.State as X
import Control.Monad.Trans.Maybe as X
import Control.Lens
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Traversable
import Data.Foldable
import GeneralsIO.Events (GameStart, GameUpdate)
import qualified GeneralsIO.Events as Evt
import qualified Data.Aeson as Json


data Owner where
  Player  :: PlayerId -> Owner
  Neutral :: Owner
  deriving (Generic, Show, Eq)

data Army = Army
  { owner :: Owner
  , size  :: Int
  }
  deriving (Generic, Show)

data Tile where
  Clear        :: Army -> Tile
  Mountain     :: Tile
  Swamp        :: Army -> Tile
  City         :: Army -> Tile
  General      :: Army -> Tile
  Fog_Clear    :: Tile
  Fog_Obstacle :: Tile
  deriving (Generic, Show)


-- update to include inferences
data GameState = GameState
  { gameStart :: GameStart
  -- ^ readonly info
  , generals :: LookupByPlayerId GridIndex
  -- ^ updates when: general is revealed, or when a player dies
  , grid     :: Grid
  }
  deriving (Generic)

mkGameState gameStart = GameState { gameStart, generals = mempty, grid = mempty}

newtype GridIndex = GridIndex Int
newtype PlayerId = PlayerId Int
  deriving (Show, Eq)

newtype ChatRoomId = ChatRoomId Text
  deriving (Show, Eq)

newtype Grid = Grid (IntMap Tile)
  deriving (Monoid, Semigroup)

newtype LookupByPlayerId a = LookupByPlayerId (Vector a)
  deriving (Monoid, Semigroup)



applyGameUpdate :: MonadState GameState m => GameUpdate -> m ()
applyGameUpdate event = do
  let diff = event ^. #mapDiff
  let
    mapTiles :: IntMap Int
    mapTiles = patch & flip evalState (diff, 0, mempty)
  -- #grid .


  pure ()

-- | diff, index, grid
type PatchState = ([Int], Int, IntMap Int)
type PatchConstraints m = (MonadState PatchState m)

patch :: forall m. PatchConstraints m => m (IntMap Int)
patch = do
  result <- runMaybeT $ forever $ applySegment
  case result of
    Nothing -> use _3
    _ -> error "forever should only terminate with Nothing"

  where
    applySegment :: MaybeT m ()
    applySegment = do
      -- peel two
      -- doubles as termination condition.
      -- make use of MaybeT's Monad fail instance here.
      (useOld : useDiff : diff) <- use _1

      -- split the diff
      let (apply, rest) = splitAt useDiff diff
      _1 .= rest

      -- update the index
      i <- _2 <+= useOld

      -- insert new tiles
      for_ (apply `zip` [i, i+1 ..]) $ \(tile, i) ->
        _3 . ix i .= tile

      _2 += useDiff
