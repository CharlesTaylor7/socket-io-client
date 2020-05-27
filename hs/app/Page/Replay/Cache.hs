module Page.Replay.Cache
 ( toHistory
 )
 where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.Vector (Vector)
import Data.Default

import Types (Dimensions, width, height)

toHistory :: Replay -> Vector Grid
toHistory replay = fromList $
  scanl
    (flip $ nextGrid)
    (initialGrid replay)
    (replay ^. moves . to turns)

type Turn = (Int, [Move])
type Turns = [Turn]

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


ixGrid :: GridIndex -> Traversal' Grid Tile
ixGrid = ix . coerce

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
applyMoves moves grid = foldl' (flip moveReducer) grid moves

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

moveReducer :: Move -> Grid -> Grid
moveReducer move grid =
  let
    unsafeArmyLens coords = singular (ixGrid coords . _Army)

    (tileArmy, grid') = grid
      & unsafeArmyLens (move ^. startTile)
      <<%~ over size (leaveArmySize $ move ^. onlyHalf)

    attackingPlayer = tileArmy ^. owner
    attackingArmySize = moveArmySize (move ^. onlyHalf) (tileArmy ^. size)
    attackingArmy = attackingPlayer `Army` attackingArmySize
  in
    grid'
    & unsafeArmyLens (move ^. endTile)
    %~ attack attackingArmy


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
