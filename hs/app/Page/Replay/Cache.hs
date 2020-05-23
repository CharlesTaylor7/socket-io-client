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

commandReducer :: Replay -> Command -> Cache -> Cache
commandReducer replay@Replay{..} command cache =
  let
    Turns {..} = toTurns replay
    (turnIndex, cache') = cache
      & currentIndex <%~ clamp 0 maxTurn . (+ (update command))
    nextGrid =
      case turnsMap ^. at turnIndex of
        Just turn -> turnReducer turn
        Nothing -> identity
  in
    -- the cache already has the index
    if is _Just $ cache' ^. history . at turnIndex
    then cache'
    else cache'
      & history . at turnIndex
      ?~ nextGrid (currentGrid cache)

turnReducer :: Turn -> Grid -> Grid
turnReducer turn grid = foldl' (flip moveReducer) grid turn

moveReducer :: Move' -> Grid -> Grid
moveReducer Move' {..} grid =
  let
    unsafeArmyLens coords = singular (ix coords . _Army)

    (tileArmy, grid') = grid
      & unsafeArmyLens startTile
      <<%~ over size (if onlyHalf then (`div` 2) else const 1)

    attackingPlayer = tileArmy ^. owner
    attackingArmySize = tileArmy ^. size &
      if onlyHalf
      then uncurry (+) . (`divMod` 2)
      else subtract 1

  in
    grid'
    & unsafeArmyLens endTile .~ attackingPlayer `Army` attackingArmySize
