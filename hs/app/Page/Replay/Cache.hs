module Page.Replay.Cache where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.Map (lookupMax)

import Types (Dimensions, width, height)


newCache :: Grid -> Cache
newCache tiles = Cache 0 $ fromList [tiles]

maxKeyOr :: Ord k => Map k a -> k -> k
maxKeyOr map def = maybe def identity $
  map ^? to lookupMax . _Just . _1

currentGrid :: Cache -> Grid
currentGrid cache = cache ^?! history . ix (cache ^. currentIndex)

update Forwards = (+ 1)
update Backwards = subtract 1
update (JumpTo n) = const n

toTurns :: Replay -> Turns
toTurns replay = Turns (map `maxKeyOr` 0) map
  where
    map = fromList $
      [ (turnIndex, moves')
      | moveGroup <- groupWith (view turn) (replay ^. moves)
      , let turnIndex = moveGroup ^. head1 . turn
      , let moves' = moveGroup <&> convertMove
      ]

convertMove :: Move -> Move'
convertMove move = Move'
  { _startTile = move ^. startTileIndex . coerced
  , _endTile   = move ^. endTileIndex   . coerced
  , _onlyHalf  = move ^. is50
  }

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

commandReducer :: (Replay, Turns) -> Command -> Cache -> Cache
commandReducer (replay, turns) command cache =
  let
    (turnIndex, cache') = cache
      & currentIndex <%~ clamp 0 (turns^.maxTurn) . update command

    cacheBound = (cache ^. history . to length) - 1

    turnDiff = turnIndex - cacheBound
  in
    if turnDiff <= 0
    then cache'
    else cache'
      & history <>~ (fromList $
        scanl
          (flip $ nextGrid turns)
          (currentGrid cache)
          [cacheBound+1..turnDiff]
      )

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

moveReducer :: Move' -> Grid -> Grid
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
