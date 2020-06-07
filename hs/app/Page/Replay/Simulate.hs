module Page.Replay.Simulate
 ( toHistory
 )
 where

import Page.Replay.Simulate.Types
import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Data.Default

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import qualified Data.IntSet as Set

import qualified Data.Vector as Vector
import qualified Data.Vector.Generic.Mutable as MVector
import Data.Vector.Fusion.Bundle (MBundle)
import qualified Data.Vector.Fusion.Bundle.Monadic as Stream

import Control.Monad.Primitive (PrimMonad)

unstream :: (PrimMonad m) => MBundle m v a -> m (Vector a)
unstream = Vector.freeze <=< MVector.munstream


toHistory
  :: MonadIO m
  => Replay
  -> m (Vector Grid)
toHistory replay =
  Stream.unfoldrM
    (\(gameInfo, turns) ->
      case turns of
        t:ts -> runMaybeT $ do
          gameInfo <- tryTo $ advanceTurn t gameInfo
          pure (gameInfo, (gameInfo, ts))
        [] ->
          pure Nothing
    )
    (seed, replay ^. replay_moves . to turns)
  & Stream.cons seed
  & fmap (view gameInfo_grid)
  & unstream
  & liftIO

  where
    seed :: GameInfo
    seed = initialGameInfo replay

    tryTo :: a -> MaybeT IO a
    tryTo = MaybeT . fmap (preview _Right) . try' . evaluate

    try' :: IO a -> IO (Either SomeException a)
    try' = try

turns :: [Move] -> Turns
turns moves = unfoldr f (1, moves)
  where
    f (i, moves) =
      if null moves
      then Nothing
      else Just $
        let (group, rest) = moves & span ((i ==) . view move_turn)
        in ((i, group), (i + 1, rest))

initialGameInfo :: Replay -> GameInfo
initialGameInfo replay = GameInfo
  { _gameInfo_grid = map
  , _gameInfo_activeCities = fromList $ replay ^.. replay_generals . folded
  , _gameInfo_activeSwamps = mempty
  , _gameInfo_owned = fromList $ replay ^.. replay_generals . ifolded . withIndex  . alongside identity (to singleton)
  }
  where
    singleton :: Int -> IntSet
    singleton tile = mempty & contains tile .~ True
    map = fold
      [ mountainsMap
      , citiesMap
      , generalsMap
      , swampsMap
      , clearMap
      ]
    mountainsMap = fromList $
      [ (index, Mountain)
      | index <- replay ^.. replay_mountains . folded
      ]

    citiesMap = fromList $
      [ (index, City (Neutral `Army` fromIntegral size))
      | (index, size) <- zip (replay ^.. replay_cities . folded) (replay ^.. replay_cityArmies . folded)
      ]

    generalsMap = fromList $
      [ (boardIndex, General $ Player playerId `Army` 1)
      | (playerId, boardIndex) <- replay ^.. replay_generals . folded . withIndex
      ]

    swampsMap = fromList $
      [ (boardIndex, Swamp def)
      | boardIndex <- replay ^.. replay_swamps . folded
      ]

    clearMap = fromList $
      [ (i, def)
      | i <- [0..numTiles - 1]
      ]
    numTiles = replay^.replay_mapWidth * replay^.replay_mapHeight

advanceTurn :: Turn -> GameInfo -> GameInfo
advanceTurn (turnIndex, moves) =
  cityGrowth turnIndex .
  tileGrowth turnIndex .
  (execState $ swampLoss turnIndex) .
  applyMoves moves

increment i = singular (ix i . _Army . army_size) +~ 1

cityGrowth :: Int -> GameInfo -> GameInfo
cityGrowth turnIndex info =
  if turnIndex `mod` 2 == 1
  then
    info
    & gameInfo_grid .~
      foldl'
        (flip increment)
        (info ^. gameInfo_grid)
        (info ^. gameInfo_activeCities . to Set.toList)
  else
    info

tileGrowth :: Int -> GameInfo -> GameInfo
tileGrowth turnIndex info =
  if turnIndex `mod` 50 == 49
  then
    info
    & gameInfo_grid .~
      foldl'
        (flip increment)
        (info ^. gameInfo_grid)
        (info ^. gameInfo_owned . folded . to Set.toList)
  else
    info

swampLoss :: MonadState GameInfo m => Int -> m ()
swampLoss turnIndex =
  when (turnIndex `mod` 2 == 1) $ do
    replay_swamps <- use $ gameInfo_activeSwamps . to Set.toList
    for_ replay_swamps $ \i -> do
      updatedArmy <- gameInfo_grid . ixLens i . singular _Army <%=
        \army ->
          if army^.army_size > 1
          then army & army_size -~ 1
          else Neutral `Army` 0
      when (is _Neutral $ updatedArmy ^. army_owner) $
        gameInfo_activeSwamps . contains i .= False

applyMoves :: [Move] -> GameInfo -> GameInfo
applyMoves moves info =
  traverse_ moveReducer moves
  &   runWriterT
  >>= traverse_ applyKill . view _2
  &   flip execState info

ixLens :: (Ixed s) => Index s -> Lens' s (IxValue s)
ixLens = singular . ix

applyKill :: MonadState GameInfo m => Kill -> m ()
applyKill (Kill killer target) = do
  -- remove all territory belonging to target
  maybe_territory <- gameInfo_owned . at target <<.= Nothing
  let
    territory = case maybe_territory of
      Just territory -> territory
      Nothing -> error $ "player " <> show target <> " cannot be killed twice"

  -- give it to killer
  gameInfo_owned . ixLens killer <>= territory

  -- halve the armies in the transferred territory
  for_ (Set.toList territory) $ \i ->
    gameInfo_grid . ix i . _Army . army_size %= halfRoundUp


moveReducer
  :: (MonadState GameInfo m, MonadWriter [Kill] m)
  => Move
  -> m ()
moveReducer move = do
  let
    attackingTile move = ixGrid (move ^. move_startTile)
    defendingTile move = ixGrid (move ^. move_endTile)

  -- subtract army from attacking tile
  tileArmy <-
    gameInfo_grid . attackingTile move . singular _Army
    <<%= over army_size (leaveArmySize $ move ^. move_onlyHalf)

  -- build attacking army
  let
    attackingPlayer = tileArmy ^. army_owner
    attackingArmySize = moveArmySize (move ^. move_onlyHalf) (tileArmy ^. army_size)
    attackingArmy = attackingPlayer `Army` attackingArmySize

  -- determine defending player
  defendingPlayer <- use (gameInfo_grid . defendingTile move . singular _Army . army_owner)

  -- army leftover after attack
  newArmy <-
    gameInfo_grid . defendingTile move . singular _Army
    <%= attack attackingArmy

  let newOwner = newArmy ^. army_owner
  defendingTileWasGeneral <- use (gameInfo_grid . ixGrid (move ^. move_endTile) . to (is _General))

  -- update cache
  when (newOwner /= defendingPlayer) $ do
    let newPlayerId =
          case newOwner ^? _Player of
            Just id -> id
            Nothing -> error $ show (move ^. move_endTile, newArmy)
    gameInfo_owned . ixLens newPlayerId . contains (move ^. move_endTile . _GridIndex) .= True
    case defendingPlayer ^? _Player of
      Just id ->
        gameInfo_owned . ixLens id . contains (move ^. move_endTile . _GridIndex) .= False
      _ -> do
        defenseTileType <- use (gameInfo_grid . ixGrid (move ^. move_endTile) . armyTileType)
        when (defenseTileType == Swamp_Tile) $
          gameInfo_activeSwamps . contains (move ^. move_endTile . _GridIndex) .= True
        when (defenseTileType == City_Tile) $
          gameInfo_activeCities . contains (move ^. move_endTile . _GridIndex) .= True

  -- emit kill & convert conquered tile to being a city
  when (newOwner /= defendingPlayer && defendingTileWasGeneral) $ do
    tell $ pure $ Kill
      { kill_killer = newOwner ^?! _Player
      , kill_target = defendingPlayer ^?! _Player
      }
    gameInfo_grid . ixGrid (move ^. move_endTile) . armyTileType .= City_Tile

  pure ()

halfRoundUp :: Integral n => n -> n
halfRoundUp = uncurry (+) . (`divMod` 2)

halfRoundDown :: Integral n => n -> n
halfRoundDown = (`div` 2)

leaveArmySize :: Integral n => Bool -> n -> n
leaveArmySize move_onlyHalf =
  if move_onlyHalf
  then halfRoundDown
  else const 1

moveArmySize :: Integral n => Bool -> n -> n
moveArmySize move_onlyHalf =
  if move_onlyHalf
  then halfRoundUp
  else subtract 1


attack :: Army -> Army -> Army
attack attacking defending
  | attacking ^. army_owner == defending ^. army_owner
  = defending & army_size +~ attacking ^. army_size

  | attacking ^. army_size > defending ^. army_size
  = attacking & army_size -~ defending ^. army_size

  | otherwise
  = defending & army_size -~ attacking ^. army_size
