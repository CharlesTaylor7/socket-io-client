module Page.Replay.Simulate
 ( toHistory
 )
 where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Prelude hiding (scanl)
import Data.Vector (Vector, scanl')
import Data.Default

import Types (Dimensions, width, height)

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import Data.IntSet (IntSet)
import qualified Data.Vector.Unboxed as U

instance

data Kill = Kill
  { kill_killer :: Int
  , kill_target :: Int
  }

type MonadKills m = MonadWriter [Kill] m

type Turn = (Int, [Move])
type Turns = [Turn]

-- data Cache = Cache
--   { cities :: U.Vector Int
--   , swamps :: U.Vector Int
--   , owned :: IntSet
--   }

toHistory :: Replay -> Vector Grid
toHistory replay =
  scanl'
    (flip $ nextGrid)
    (initialGrid replay)
    (fromList . turns $ replay ^. moves)

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
initialGrid replay = fold
  [ mountainsMap
  , citiesMap
  , generalsMap
  , swampsMap
  , clearMap
  ]
  where
    mountainsMap = fromList $
      [ (index, Mountain)
      | index <- replay ^.. mountains . folded
      ]

    citiesMap = fromList $
      [ (index, City (Neutral `Army` fromIntegral size))
      | (index, size) <- zip (replay ^.. cities . folded) (replay ^.. cityArmies . folded)
      ]

    generalsMap = fromList $
      [ (boardIndex, General $ Player playerId `Army` 1)
      | (playerId, boardIndex) <- replay ^.. generals . folded . withIndex
      ]

    swampsMap = fromList $
      [ (boardIndex, Swamp def)
      | boardIndex <- replay ^.. swamps . folded
      ]

    clearMap = fromList $
      [ (i, def)
      | i <- [0..numTiles - 1]
      ]
    numTiles = replay^.mapWidth * replay^.mapHeight


nextGrid :: Turn -> Grid -> Grid
nextGrid (turnIndex, moves) =
  cityGrowth turnIndex .
  tileGrowth turnIndex .
  swampLoss  turnIndex .
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
    _Army .
    match (owner . _Player) .
    size +~ 1
  else identity

swampLoss :: Int -> Grid -> Grid
swampLoss turnIndex =
  if turnIndex `mod` 2 == 1
  then
    traversed . _Swamp %~
    \army ->
      if army^.size > 1
      then army & size -~ 1
      else Neutral `Army` 0
  else identity

applyMoves :: [Move] -> Grid -> Grid
applyMoves moves grid = grid''
  where
    (grid', kills) = traverse_ moveReducer moves
      & flip execStateT grid
      & runWriter
    grid'' = foldl' (flip applyKill) grid' kills

applyKill :: Kill -> Grid -> Grid
applyKill (Kill killer target) grid = grid
  & traversed . _Army %~
    (\army -> army
      & match (matchOnTarget) . size %~ uncurry (+) . (`divMod` 2)
      & matchOnTarget .~ killer
    )
  where
    matchOnTarget :: Traversal' Army Int
    matchOnTarget =owner . _Player . filtered (== target)

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

  let
    attackingPlayer = tileArmy ^. owner
    attackingArmySize = moveArmySize (move ^. onlyHalf) (tileArmy ^. size)
    attackingArmy = attackingPlayer `Army` attackingArmySize

  defendingPlayer <- use (unsafeArmyLens (move ^. endTile) . owner)

  newArmy <-
    unsafeArmyLens (move ^. endTile)
    <%= attack attackingArmy

  let newOwner = newArmy ^. owner
  defendingTileWasGeneral <- use (ixGrid (move ^. endTile) . to (is _General))

  when (newOwner /= defendingPlayer && defendingTileWasGeneral) $ do
    tell $ pure $ Kill
      { kill_killer = newOwner ^?! _Player
      , kill_target = defendingPlayer ^?! _Player
      }
    ixGrid (move ^. endTile) . armyTileType .= City_Tile

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
  | attacking ^. owner == defending ^. owner
  = defending & size +~ attacking ^. size

  | attacking ^. size > defending ^. size
  = attacking & size -~ defending ^. size

  | otherwise
  = defending & size -~ attacking ^. size
