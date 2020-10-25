# generals-bot

## Install for development
- clone repo
- cabal build
- cabal run

## To do

### Features
- style replay toggle to make it clear which perspective is active
- handle afks / surrenders

#### Replays
- replay management
  - delete a replay
  - import a replay
- loading animation while replay loads
- advance replay back & forward
- controls menu

#### Bots
- Port old bot client to Haskell
- Hot reloading of bot strategies (strategies in haskell)

### Debugging
- debug menu for showing the game info cache
- replace unsafe lens with alternatives that run in MonadError
