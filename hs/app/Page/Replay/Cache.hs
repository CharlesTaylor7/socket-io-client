module Page.Replay.Cache where

import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.Map (lookupMax)

import Types (Dimensions, width, height)


newCache :: Grid -> Cache
newCache tiles = Cache 0 $ fromList [(0, tiles)]

maxKeyOr :: Ord k => Map k a -> k -> k
maxKeyOr map def = maybe def identity $
  map ^? to lookupMax . _Just . _1

currentGrid :: Cache -> Grid
currentGrid Cache {..} = _history ^?! ix _currentIndex

update Forwards = 1
update Backwards = -1

toTurns :: Replay -> Turns
toTurns Replay {..} = Turns (map `maxKeyOr` 0) map
  where
    map = fromList $
      [ (turnIndex, moves')
      | moveGroup <- groupWith turn moves
      , let turnIndex = moveGroup & head & turn
      , let moves' = moveGroup <&> convertMove (dimensions ^. width)
      ]

convertMove :: Int -> Move -> Move'
convertMove rowLength Move {..} = Move'
  { startTile = toCoords startTileIndex
  , endTile   = toCoords endTileIndex
  , onlyHalf  = is50
  }
  where
    toCoords = view $ coordinated rowLength

clamp :: Ord a => a -> a -> a -> a
clamp min max x
  | x < min   = min
  | x > max   = max
  | otherwise = x

increment :: (Int, Int) -> Grid -> Grid
increment i = ix i . _Army . size +~ 1

commandReducer :: (Replay, Turns) -> Command -> Cache -> Cache
commandReducer (Replay {..}, Turns{..}) command cache =
  -- the cache already has the turn index
  if is _Just $ cache' ^. history . at turnIndex
  then cache'
  else cache'
    & history . at turnIndex
    ?~ nextGrid (currentGrid cache)
  where
    (turnIndex, cache') = cache
      & currentIndex <%~ clamp 0 maxTurn . (+ (update command))

    nextGrid :: Grid -> Grid
    nextGrid = cityGrowth . tileGrowth . makeMoves

    mapWidth = dimensions ^. width

    makeMoves = maybe identity turnReducer $ turnsMap ^. at turnIndex

    cityGrowth = appEndo $
       (generals <> cities) ^.
       folded . coordinated mapWidth . to increment . _Unwrapped

    tileGrowth =
      if turnIndex `mod` 50 == 0
      then traversed . _Clear . size +~ 1
      else identity

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
moveReducer Move' {..} grid =
  let
    unsafeArmyLens coords = singular (ix coords . _Army)

    (tileArmy, grid') = grid
      & unsafeArmyLens startTile
      <<%~ over size (leaveArmySize onlyHalf)

    attackingPlayer = tileArmy ^. owner
    attackingArmySize = tileArmy ^. size & moveArmySize onlyHalf


  in
    grid'
    & unsafeArmyLens endTile .~ attackingPlayer `Army` attackingArmySize
