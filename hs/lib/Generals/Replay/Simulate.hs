module Generals.Replay.Simulate
  ( toHistory
  )
  where

import Generals.Types

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
    (seed, replay ^. #moves . to turns)
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
        let (group, rest) = moves & span ((i ==) . view #turn)
        in ((i, group), (i + 1, rest))

initialGameInfo :: Replay -> GameInfo
initialGameInfo replay = GameInfo
  { grid = map
  , activeCities = fromList $ replay ^.. #generals . folded
  , activeSwamps = mempty
  , owned = fromList $ replay ^.. #generals . ifolded . withIndex  . alongside identity (to singleton)
  , kills = []
  , numTiles
  , replay
  , turnIndex = TurnIndex 0
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
      | index <- replay ^.. #mountains . folded
      ]

    citiesMap = fromList $
      [ (index, City (Neutral `Army` fromIntegral size))
      | (index, size) <- zip (replay ^.. #cities . folded) (replay ^.. #cityArmies . folded)
      ]

    generalsMap = fromList $
      [ (boardIndex, General $ Player playerId `Army` 1)
      | (playerId, boardIndex) <- replay ^.. #generals . folded . withIndex
      ]

    swampsMap = fromList $
      [ (boardIndex, Swamp def)
      | boardIndex <- replay ^.. #swamps . folded
      ]

    clearMap = fromList $
      [ (i, def)
      | i <- [0..numTiles - 1]
      ]
    numTiles = replay ^. #mapWidth * replay ^. #mapHeight


advanceTurn :: SimulateMonadConstraints m => Turn -> m ()
advanceTurn (turnIndex, moves) = do
  #turnIndex . _TurnIndex .= turnIndex
  applyMoves moves
  cityGrowth turnIndex
  tileGrowth turnIndex
  swampLoss turnIndex


increment :: Int -> Grid -> Grid
increment i = singular
  ("incrementing army at index: " <> show i)
  (#_Grid . ix i . _Army . #size)
  +~ 1


cityGrowth :: SimulateMonadConstraints m => Int -> m ()
cityGrowth turnIndex =
  when (turnIndex `mod` 2 == 1) $ do
    grid <- use #grid
    activeCities <- use #activeCities
    #grid .=
      Set.foldl'
        (flip increment)
        grid
        activeCities

tileGrowth :: SimulateMonadConstraints m => Int -> m ()
tileGrowth turnIndex =
  when (turnIndex `mod` 50 == 49) $ do
    grid <- use #grid
    owned <- use $ #owned . folded
    #grid .=
      Set.foldl'
        (flip increment)
        grid
        owned

swampLoss :: MonadState GameInfo m => Int -> m ()
swampLoss turnIndex =
  when (turnIndex `mod` 2 == 1) $ do
    swamps <- use $ #activeSwamps . to Set.toList
    for_ swamps $ \i -> do
      let
        setter = singular
          ("swamp decrementing army at index: " <> show i)
          (#grid . #_Grid . ix i . _Army)
      updatedArmy <- setter <%=
        \army ->
          if army ^. #size > 1
          then army & #size -~ 1
          else Neutral `Army` 0
      when (is #_Neutral $ updatedArmy ^. #owner) $
        #activeSwamps . contains i .= False

applyMoves :: SimulateMonadConstraints m => [Move] -> m ()
applyMoves moves =
  traverse_ moveReducer moves
  &   runWriterT
  >>= traverse_ applyKill . view _2

applyKill :: SimulateMonadConstraints m => Kill -> m ()
applyKill kill = do
  let killer = kill ^. #killer
  let target = kill ^. #mark

  -- remove all territory belonging to target
  maybe_territory <- #owned . at target <<.= Nothing

  territory <-  maybe_territory ^?? _Just $
    "player " <> show target <> " cannot be killed twice"

  -- give it to killer
  singular
    "give territory to killer"
    (#owned . ix killer)
    <>= territory

  -- halve the armies & transfer ownership
  for_ (Set.toList territory) $ \i ->
    #grid . #_Grid . ix i . _Army %=
      ( over #size halfRoundUp
      . set (#owner . #_Player) killer
      )

  -- prepend kill to game log
  #kills %= (kill :)

moveReducer
  :: (SimulateMonadConstraints m, MonadWriter [Kill] m)
  => Move
  -> m ()
moveReducer move = do
  let
    attackingTile move = ixGrid (move ^. #startTile)
    defendingTile move = ixGrid (move ^. #endTile)

  -- subtract army from attacking tile
  tileArmy <-
    singular
      ("attacking tile army")
      (#grid . attackingTile move . _Army)
    <<%= over #size (leaveArmySize $ move ^. #onlyHalf)

  -- build attacking army
  let
    attackingPlayer = tileArmy ^. #owner
    attackingArmySize = moveArmySize (move ^. #onlyHalf) (tileArmy ^. #size)
    attackingArmy = attackingPlayer `Army` attackingArmySize

  -- determine defending player
  defendingPlayer <- use $
    singular
      "defending tile army"
      (#grid . defendingTile move . _Owner)

  -- army leftover after attack
  newArmy <-
    singular
      "army leftover after attack"
      (#grid . defendingTile move . _Army)
    <%= attack attackingArmy

  let newOwner = newArmy ^. #owner
  defendingTileWasGeneral <- use (#grid . ixGrid (move ^. #endTile) . to (is #_General))

  -- update cache
  when (newOwner /= defendingPlayer) $ do
    newPlayerId <- newOwner ^?? #_Player $
      show (move ^. #endTile, newArmy)

    #owned . ix newPlayerId . contains (move ^. #endTile . _GridIndex) .= True
    case defendingPlayer ^? #_Player of
      Just id ->
        #owned . ix id . contains (move ^. #endTile . _GridIndex) .= False
      _ -> do
        defenseTileType <- use (#grid . ixGrid (move ^. #endTile) . armyTileType)
        when (defenseTileType == Swamp_Tile) $
          #activeSwamps . contains (move ^. #endTile . _GridIndex) .= True
        when (defenseTileType == City_Tile) $
          #activeCities . contains (move ^. #endTile . _GridIndex) .= True

  -- emit kill & convert conquered tile to being a city
  when (newOwner /= defendingPlayer && defendingTileWasGeneral) $ do
    -- read player ids
    killer <- newOwner ^?? #_Player $ "Kill: killer"
    mark <- defendingPlayer ^?? #_Player $ "Kill: mark"
    -- emit kill
    tell $ singleton $ Kill { killer, mark, turn = move ^. #turn }
    -- convert conquered general to a city
    #grid . ixGrid (move ^. #endTile) . armyTileType .= City_Tile

  pure ()


singleton :: a -> [a]
singleton = pure

halfRoundUp :: Integral n => n -> n
halfRoundUp = uncurry (+) . (`divMod` 2)

halfRoundDown :: Integral n => n -> n
halfRoundDown = (`div` 2)

leaveArmySize :: Integral n => Bool -> n -> n
leaveArmySize onlyHalf =
  if onlyHalf
  then halfRoundDown
  else const 1

moveArmySize :: Integral n => Bool -> n -> n
moveArmySize onlyHalf =
  if onlyHalf
  then halfRoundUp
  else subtract 1


attack :: Army -> Army -> Army
attack attacking defending
  | attacking ^. #owner == defending ^. #owner
  = defending & #size +~ attacking ^. #size

  | attacking ^. #size > defending ^. #size
  = attacking & #size -~ defending ^. #size

  | otherwise
  = defending & #size -~ attacking ^. #size


applyPerspective
  :: Perspective
  -> GameInfo
  -> Grid
applyPerspective Global gameInfo =
  gameInfo ^. #grid

applyPerspective (Perspective playerId) gameInfo =
  let
    -- check tile & each of its 8 neighbors in the player owned cached
    allTiles :: IntSet
    allTiles = fromList [0 .. gameInfo ^. #numTiles - 1]

    owned :: IntSet
    owned = gameInfo ^. #owned . ix playerId

    visible :: IntSet
    visible = owned ^. members . to visibleFrom . to fromList

    visibleFrom :: Int -> [Int]
    visibleFrom i =
      let
        w = gameInfo ^. #replay . #mapWidth
        column = [i - w, i, i + w]
        next = column <&> (+ 1)
        prev = column <&> subtract 1
      in
        case i `rem` w of
          0              -> column <> next
          r | r == (w-1) -> column <> prev
          _              -> column <> next <> prev

    fog :: IntSet
    fog = Set.difference allTiles visible

    markFog :: Tile -> Tile
    markFog (Clear _)    = Fog_Clear
    markFog (General _)  = Fog_Clear
    markFog Fog_Clear    = Fog_Clear
    markFog Mountain     = Fog_Obstacle
    markFog (City _)     = Fog_Obstacle
    markFog (Swamp _)    = Fog_Obstacle
    markFog Fog_Obstacle = Fog_Obstacle
  in
    foldrOf
      members
      (\tile -> _Grid . ix tile %~ markFog)
      (gameInfo ^. #grid)
      fog
