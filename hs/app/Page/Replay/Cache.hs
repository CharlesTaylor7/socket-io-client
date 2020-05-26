module Page.Replay.Cache
 ( newCache
 , currentGrid
 , commandReducer
 )
 where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.IntMap (lookupMax)

import Types (Dimensions, width, height)


currentGrid :: HasCallStack => Cache -> Grid
currentGrid cache = cache ^?! cache_lookup . ix (cache ^. cache_index)


newCache :: Replay -> Grid -> Cache
newCache replay grid = Cache
  { _cache_index = 0
  , _cache_lookup = fromList $
      scanl
        (flip $ nextGrid turns)
        grid
        [1 .. turns ^. maxTurn]
  }
  where
    turns = toTurns replay

commandReducer :: Command -> Cache -> Cache
commandReducer command cache =
  case command of
    Backwards -> cache & cache_index %~ max 0 . subtract 1
    Forwards  -> cache & cache_index %~ min lastTurn . (+ 1)
    JumpTo n  -> cache & cache_index .~ min lastTurn n
  where
    lastTurn = cache ^. cache_lookup . to length


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
