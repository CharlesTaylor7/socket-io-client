module Page.Replay.Cache
 ( toHistory
 )
 where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.Vector (Vector)
import Data.Default

import Types (Dimensions, width, height)

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

data Kill = Kill
  { kill_by     :: Int
  , kill_target :: Int
  }

type MonadKills m = MonadWriter [Kill] m

type Turn = (Int, [Move])
type Turns = [Turn]


toHistory :: Replay -> Vector Grid
toHistory replay = fromList $
  scanl
    (flip $ nextGrid)
    (initialGrid replay)
    (replay ^. moves . to turns)

turns :: [Move] -> Turns
turns moves = unfoldr f (1, moves)
  where
    f (i, moves) =
      if null moves
      then Nothing
      else Just $
        let (group, rest) = moves & span ((i ==) . view turn)
        in ((i, group), (i + 1, rest))

initialGrid :: Replay -> Grid
initialGrid replay = mountainsMap <> citiesMap <> generalsMap <> clearMap
  where
    mountainsMap = fromList $
      [ (index, Mountain)
      | index <- replay ^. mountains
      ]

    citiesMap = fromList $
      [ (index, City (Neutral `Army` fromIntegral size))
      | (index, size) <- zip (replay ^. cities) (replay ^. cityArmies)
      ]

    generalsMap = fromList $
      [ (boardIndex, General $ Player playerId `Army` 1)
      | (playerId, boardIndex) <- replay ^.. generals . folded . withIndex
      ]

    clearMap = fromList $
      [ (i, def)
      | i <- [0..numTiles - 1]
      ]
    numTiles = replay^.mapWidth * replay^.mapHeight


ixGrid :: GridIndex -> Lens' Grid Tile
ixGrid = singular . ix . coerce

increment :: GridIndex -> Grid -> Grid
increment i = ixGrid i . _Army . size +~ 1

match :: Traversal' s a -> Traversal' s s
match matcher = filtered $ is _Just . firstOf matcher

nextGrid :: Turn -> Grid -> Grid
nextGrid (turnIndex, moves) =
  cityGrowth turnIndex .
  tileGrowth turnIndex .
  applyMoves moves

cityGrowth :: Int -> Grid -> Grid
cityGrowth turnIndex =
  if turnIndex `mod` 2 == 1
  then
    traversed .
    (_City `failing` _General) .
    match (owner . _Player) .
    size +~ 1
  else identity

tileGrowth :: Int -> Grid -> Grid
tileGrowth turnIndex=
  if turnIndex `mod` 50 == 49
  then
    traversed .
    (_Clear `failing` _City `failing` _General) .
    match (owner . _Player) .
    size +~ 1
  else identity

applyMoves :: [Move] -> Grid -> Grid
applyMoves moves grid = grid'
  where
    (grid', kills) = traverse_ moveReducer moves
      & flip execStateT grid
      & runWriter

attackingTile move = ixGrid (move ^. startTile)
defendingTile move = ixGrid (move ^. startTile)

moveReducer
  :: (MonadState Grid m, MonadWriter [Kill] m)
  => Move
  -> m ()
moveReducer move = do

  let unsafeArmyLens coords = singular (ixGrid coords . _Army)

  tileArmy <-
    unsafeArmyLens (move ^. startTile)
    <<%= over size (leaveArmySize $ move ^. onlyHalf)

  defendingPlayer <- use (unsafeArmyLens (move ^. startTile) . owner)

  let
    attackingPlayer = tileArmy ^. owner
    attackingArmySize = moveArmySize (move ^. onlyHalf) (tileArmy ^. size)
    attackingArmy = attackingPlayer `Army` attackingArmySize

  newArmy <-
    unsafeArmyLens (move ^. endTile)
    <%= attack attackingArmy

  let newOwner = newArmy ^. owner
  defendingTileWasGeneral <- use (ixGrid (move ^. endTile) . to (is _General))

  when (newOwner /= defendingPlayer && defendingTileWasGeneral) $ do
    tell $ pure $ Kill
      { kill_by     = newOwner ^?! _Player
      , kill_target = defendingPlayer ^?! _Player
      }
    ixGrid (move ^. endTile) . tileType .= General_Tile

  pure ()


leaveArmySize :: Integral n => Bool -> n -> n
leaveArmySize onlyHalf =
  if onlyHalf
  then (`div` 2)
  else const 1


moveArmySize :: Integral n => Bool -> n -> n
moveArmySize onlyHalf =
  if onlyHalf
  then uncurry (+) . (`divMod` 2)
  else subtract 1


attack :: Army -> Army -> Army
attack attacking defending
  | defending ^. owner == Neutral
  =  attacking

  | attacking ^. owner == defending ^. owner
  = defending & size +~ attacking ^. size

  | attacking ^. size > defending ^. size
  = attacking & size -~ defending ^. size

  | otherwise
  = defending & size -~ attacking ^. size
