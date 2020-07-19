module Page.Replay.Simulate
 ( toHistory
 )
 where

import Page.Replay.Simulate.Types
import Page.Replay.Types hiding (Turn)
import Generals.Map.Types hiding (Map)

import Control.Lens.Unsafe (singular)

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Except

import qualified Data.IntSet as Set

import qualified Data.Vector as Vector
import qualified Data.Vector.Generic.Mutable as MVector
import Data.Vector.Fusion.Bundle (MBundle)
import qualified Data.Vector.Fusion.Bundle.Monadic as Stream

import Control.Monad.Primitive (PrimMonad)


type SimulateMonadConstraints m = (MonadError Text m, MonadState GameInfo m)


unstream :: (PrimMonad m) => MBundle m v a -> m (Vector a)
unstream = Vector.freeze <=< MVector.munstream

toHistory
  :: MonadIO m
  => Replay
  -> m (Vector GameInfo)
toHistory replay =
  Stream.unfoldrM
    (\(gameInfo, turns) ->
      case turns of
        t:ts -> runMaybeT $ do
          gameInfo <- advanceTurn t
            & flip execStateT gameInfo
            & logError
          pure (gameInfo, (gameInfo, ts))
        [] ->
          pure Nothing
    )
    (seed, replay ^. replay_moves . to turns)
    & Stream.cons seed
    & unstream
    & liftIO

  where
    seed :: GameInfo
    seed = initialGameInfo replay

    logError :: Either Text a -> MaybeT IO a
    logError = \case
      Left text -> print text >> empty
      Right a -> tryTo a

    tryTo :: a -> MaybeT IO a
    tryTo action = do
      either <- liftIO . try . evaluate $ action
      case either of
        Left (exception :: SomeException) -> do
          print exception
          empty
        Right a -> pure a


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
  , _gameInfo_gridWidth = replay ^. replay_mapWidth
  , _gameInfo_numTiles = numTiles
  }
  where
    singleton :: Int -> IntSet
    singleton tile = mempty & contains tile .~ True

    map :: Grid
    map = Grid $ fold
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


advanceTurn :: SimulateMonadConstraints m => Turn -> m ()
advanceTurn (turnIndex, moves) = do
  applyMoves moves
  cityGrowth turnIndex
  tileGrowth turnIndex
  swampLoss turnIndex

increment :: Int -> Grid -> Grid
increment i = singular
  ("incrementing army at index: " <> show i)
  (_Grid . ix i . _Army . army_size)
  +~ 1


cityGrowth :: SimulateMonadConstraints m => Int -> m ()
cityGrowth turnIndex =
  when (turnIndex `mod` 2 == 1) $ do
    grid <- use gameInfo_grid
    activeCities <- use gameInfo_activeCities
    gameInfo_grid .=
      Set.foldl'
        (flip increment)
        grid
        activeCities

tileGrowth :: SimulateMonadConstraints m => Int -> m ()
tileGrowth turnIndex =
  when (turnIndex `mod` 50 == 49) $ do
    grid <- use gameInfo_grid
    owned <- use $ gameInfo_owned . folded
    gameInfo_grid .=
      Set.foldl'
        (flip increment)
        grid
        owned

swampLoss :: MonadState GameInfo m => Int -> m ()
swampLoss turnIndex =
  when (turnIndex `mod` 2 == 1) $ do
    replay_swamps <- use $ gameInfo_activeSwamps . to Set.toList
    for_ replay_swamps $ \i -> do
      let
        setter = singular
          ("swamp decrementing army at index: " <> show i)
          (gameInfo_grid . _Grid . ix i . _Army)
      updatedArmy <- setter <%=
        \army ->
          if army^.army_size > 1
          then army & army_size -~ 1
          else Neutral `Army` 0
      when (is _Neutral $ updatedArmy ^. army_owner) $
        gameInfo_activeSwamps . contains i .= False

applyMoves :: SimulateMonadConstraints m => [Move] -> m ()
applyMoves moves =
  traverse_ moveReducer moves
  &   runWriterT
  >>= traverse_ applyKill . view _2

applyKill :: SimulateMonadConstraints m => Kill -> m ()
applyKill (Kill killer target) = do
  -- remove all territory belonging to target
  maybe_territory <- gameInfo_owned . at target <<.= Nothing

  territory <-  maybe_territory ^?? _Just $
    "player " <> show target <> " cannot be killed twice"

  -- give it to killer
  singular
    "give territory to killer"
    (gameInfo_owned . ix killer)
    <>= territory

  -- halve the armies & transfer ownership
  for_ (Set.toList territory) $ \i ->
    gameInfo_grid . _Grid . ix i . _Army %=
      ( over army_size halfRoundUp
      . set (army_owner . _Player) killer
      )


moveReducer
  :: (SimulateMonadConstraints m, MonadWriter [Kill] m)
  => Move
  -> m ()
moveReducer move = do
  let
    attackingTile move = ixGrid (move ^. move_startTile)
    defendingTile move = ixGrid (move ^. move_endTile)

  -- subtract army from attacking tile
  tileArmy <-
    singular
      ("attacking tile army")
      (gameInfo_grid . attackingTile move . _Army)
    <<%= over army_size (leaveArmySize $ move ^. move_onlyHalf)

  -- build attacking army
  let
    attackingPlayer = tileArmy ^. army_owner
    attackingArmySize = moveArmySize (move ^. move_onlyHalf) (tileArmy ^. army_size)
    attackingArmy = attackingPlayer `Army` attackingArmySize

  -- determine defending player
  defendingPlayer <- use $
    singular
      "defending tile army"
      (gameInfo_grid . defendingTile move . _Owner)

  -- army leftover after attack
  newArmy <-
    singular
      "army leftover after attack"
      (gameInfo_grid . defendingTile move . _Army)
    <%= attack attackingArmy

  let newOwner = newArmy ^. army_owner
  defendingTileWasGeneral <- use (gameInfo_grid . ixGrid (move ^. move_endTile) . to (is _General))

  -- update cache
  when (newOwner /= defendingPlayer) $ do
    newPlayerId <- newOwner ^?? _Player $
      show (move ^. move_endTile, newArmy)

    gameInfo_owned . ix newPlayerId . contains (move ^. move_endTile . _GridIndex) .= True
    case defendingPlayer ^? _Player of
      Just id ->
        gameInfo_owned . ix id . contains (move ^. move_endTile . _GridIndex) .= False
      _ -> do
        defenseTileType <- use (gameInfo_grid . ixGrid (move ^. move_endTile) . armyTileType)
        when (defenseTileType == Swamp_Tile) $
          gameInfo_activeSwamps . contains (move ^. move_endTile . _GridIndex) .= True
        when (defenseTileType == City_Tile) $
          gameInfo_activeCities . contains (move ^. move_endTile . _GridIndex) .= True

  -- emit kill & convert conquered tile to being a city
  when (newOwner /= defendingPlayer && defendingTileWasGeneral) $ do
    -- read player ids
    killer <- newOwner ^?? _Player $ "Kill: killer"
    mark <- defendingPlayer ^?? _Player $ "Kill: mark"

    -- emit kill
    tell $ singleton $ Kill
      { kill_killer = killer
      , kill_target = mark
      }
    -- convert conquered general to a city
    gameInfo_grid . ixGrid (move ^. move_endTile) . armyTileType .= City_Tile

  pure ()


singleton :: a -> [a]
singleton = pure

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
