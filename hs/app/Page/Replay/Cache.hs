module Page.Replay.Cache
 ( toHistory
 )
 where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.Vector (Vector)
import Data.Default
import Data.IntMap (lookupMax)

import Types (Dimensions, width, height)



toHistory :: Replay -> Vector Grid
toHistory replay = fromList $
  scanl
    (flip $ nextGrid turns)
    grid
    [1 .. turns ^. maxTurn]
  where
    grid = initialGrid replay
    turns = toTurns replay

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


maxKeyOr :: IntMap a -> Int -> Int
maxKeyOr map def = maybe def identity $
  map ^? to lookupMax . _Just . _1


toTurns :: Replay -> Turns
toTurns replay = Turns (map `maxKeyOr` 0) map
  where
    map :: IntMap (NonEmpty Move)
    map = fromList $
      [ (turnIndex, moveGroup)
      | moveGroup <- groupWith (view turn) (replay ^. moves)
      , let turnIndex = moveGroup ^. head1 . turn
      ]


ixGrid :: GridIndex -> Traversal' Grid Tile
ixGrid = ix . coerce

increment :: GridIndex -> Grid -> Grid
increment i = ixGrid i . _Army . size +~ 1

match :: Traversal' s a -> Traversal' s s
match matcher = filtered $ is _Just . firstOf matcher

nextGrid :: Turns -> Int -> Grid -> Grid
nextGrid turns turnIndex =
  cityGrowth turnIndex .
  tileGrowth turnIndex .
  makeMoves turns turnIndex

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


makeMoves :: Turns -> Int -> Grid -> Grid
makeMoves turns turnIndex = maybe identity turnReducer $
  turns ^? lookup . ix turnIndex


turnReducer :: Turn -> Grid -> Grid
turnReducer turn grid = foldl' (flip moveReducer) grid turn


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
