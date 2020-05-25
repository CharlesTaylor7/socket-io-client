module Page.Replay.Cache where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.IntMap (lookupMax)

import Types (Dimensions, width, height)


newCache :: Grid -> Cache
newCache tiles = Cache
  { _cache_index = initialIndex
  , _cache_lookup = fromList [tiles]
  }
  where initialIndex = 0


maxKeyOr :: IntMap a -> Int -> Int
maxKeyOr map def = maybe def identity $
  map ^? to lookupMax . _Just . _1

currentGrid :: HasCallStack => Cache -> Grid
currentGrid cache = cache ^?! cache_lookup . ix (cache ^. cache_index)

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

moveLeft :: Cache -> Cache
moveLeft = cache_index %~ max 0 . subtract 1

cache_bound = cache_lookup . to (subtract 1 . length)

moveRight :: Cache -> Cache
moveRight cache =
  if cache ^. cache_index == cache ^. cache_bound
  then error "can't move right!"
  else
    cache & cache_index +~ 1

commandReducer :: (Replay, Turns) -> Command -> Cache -> Cache
commandReducer (replay, turns) command cache =
  case command of
    Backwards -> moveLeft cache
    Forwards ->
      if turnIndex == cacheBound
      then
        let newGrid = nextGrid turns (cacheBound + 1) (currentGrid cache)
        in cache
          & cache_index +~ 1
          & cache_lookup %~ flip snoc newGrid
      else moveRight cache
    JumpTo n ->
      if n <= cacheBound
      then cache & cache_index .~ n
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
          & cache_index .~ n
          & cache_lookup <>~ grids

  where
    turnIndex = cache ^. cache_index
    cacheBound = cache ^. cache_bound


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
