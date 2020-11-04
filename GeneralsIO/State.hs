module GeneralsIO.State
  (
  )
  where


type GridIndex = (Int, Int)
type PlayerId = Int

data GameState = GameState
  { grid     :: Map GridIndex Tile
  , generals :: Map GridIndex PlayerId
  }
