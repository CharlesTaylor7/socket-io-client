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

currentGrid :: Cache -> Maybe Grid
currentGrid Cache {..} = _history ^. at _currentIndex

update Forwards = 1
update Backwards = -1

toTurns :: [Move] -> Turns
toTurns moves = Turns (map `maxKeyOr` 0) map
  where
    map = fromList $
      [ (turn . head $ moveGroup, toList moveGroup)
      | moveGroup <- groupWith turn moves
      ]

clamp :: Ord a => a -> a -> a -> a
clamp min max x
  | x < min   = min
  | x > max   = max
  | otherwise = x

commandReducer :: Replay -> Command -> Cache -> Cache
commandReducer Replay{..} command cache =
  let
    Turns{..} = toTurns moves
    (index, cache') = cache
      & currentIndex <%~ clamp 0 maxTurn . (+ (update command))
  in
    -- the cache already has the index
    if is _Just $ cache' ^. history . at index
    then cache'
    else
      case lookup ^. at index of
        Nothing -> cache'
        Just turn -> cache'
          & history . at index
          ?~ turnReducer turn (cache' ^?! history . ix index)

coordinated :: Int -> Iso' Int (Int, Int)
coordinated rowLength = iso to from
  where
    to index =
      let (j, i) = index `divMod` rowLength
      in (i+1, j+1)
    from (i, j) =
      (j - 1) * rowLength + (i - 1)

turnReducer :: Turn -> Grid -> Grid
turnReducer turn grid = foldl' (flip moveReducer) grid turn

moveReducer :: Move -> Grid -> Grid
moveReducer Move {..} grid =
  let
  in grid
    & identity
