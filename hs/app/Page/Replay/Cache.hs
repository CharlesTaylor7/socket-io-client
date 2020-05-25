module Page.Replay.Cache where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.IntMap (lookupMax)

import Types (Dimensions, width, height)


newCache :: Grid -> Cache
newCache tiles = Cache
  { _cache_maxIndex = initialIndex
  , _cache_zipper = fromList [tiles]
    & zipper
    & fromWithin (ix initialIndex)
  }
  where initialIndex = 0


maxKeyOr :: IntMap a -> Int -> Int
maxKeyOr map def = maybe def identity $
  map ^? to lookupMax . _Just . _1

currentGrid :: Cache -> Grid
currentGrid = view $ cache_zipper . focus

toTurns :: Replay -> Turns
toTurns replay = Turns (map `maxKeyOr` 0) map
  where
    map :: IntMap (NonEmpty Move)
    map = fromList $
      [ (turnIndex, moveGroup)
      | moveGroup <- groupWith (view turn) (replay ^. moves)
      , let turnIndex = moveGroup ^. head1 . turn
      ]

clamp :: Ord a => a -> a -> a -> a
clamp min max x
  | x < min   = min
  | x > max   = max
  | otherwise = x

ixGrid :: GridIndex -> Traversal' Grid Tile
ixGrid = ix . coerce


increment :: GridIndex -> Grid -> Grid
increment i = ixGrid i . _Army . size +~ 1

match :: Traversal' s a -> Traversal' s s
match matcher = filtered $ is _Just . firstOf matcher

unsafeView = (^?!)

commandReducer :: (Replay, Turns) -> Command -> Cache -> Cache
commandReducer (replay, turns) command cache =
  case command of
    Backwards -> cache & cache_zipper %~ tug leftward
    Forwards ->
      if turnIndex == cacheBound
      then cache
        & cache_maxIndex +~ 1
        & cache_zipper %~
            fromWithin (ix (cacheBound + 1)) .
            (focus %~ cons
              (nextGrid turns (cacheBound + 1) (currentGrid cache))
            ) .
            upward
      else cache & cache_zipper %~ tug rightward
    JumpTo n ->
      if n <= cacheBound
      then cache
        & cache_zipper
        %~ \z -> (moveTo n z) ^?! _Just
      else
        let
          grids :: Seq Grid
          grids = fromList . tail $
            scanl
              (flip $ nextGrid turns)
              (currentGrid cache)
              [cacheBound+1..n]
        in
          cache
          & cache_maxIndex .~ n
          & cache_zipper %~
            fromWithin (ix n) .
            (focus <>~ grids) .
            upward

  where
    turnIndex = cache ^. cache_zipper . to tooth
    cacheBound = cache ^. cache_maxIndex


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
