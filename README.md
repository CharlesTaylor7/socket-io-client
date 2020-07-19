# generals-bot

## Install for development
- clone repo
- init submodules
- run `yarn` to install node dependencies
- run `./scripts/shell.sh` this step will take a while the first time.
- run `./scripts/build-hs.sh` in the nix-shell. then manually copy static ghcjs dependenices listed in index.html to the right place.

## Running the dev server
- open a WSL terminal. Run `./scripts/shell.sh`. Then inside the virtual reflex shell, run `./scripts/watch-hs.sh`
- open any terminal. Run `yarn watch:server`.
- open another terminal. Run `yarn watch:js`.

Congrats, you should have built all the sources and have a server running at localhost:8000.

## To do
### Bugs
  - debug menu for showing the game info cache
- enforce uniform tile size (empty tiles get squished on zoom out)

### Features
- make control panel not an eyesore
- get jsaddle-warp + ghci working
  - remove dependencies on js ffi and external scripts
  - don't rely on browser cache or service worker
  - Cache replays at the app level
- replace unsafe lens with alternatives that run in MonadError
- View replays from global perspective
- view replays from player perspectives
- ui for importing a replay? paste a url in an input?
- loading animation while replay loads
- handle afks
- Port old bot client to Haskell
- Hot reloading of bot strategies (strategies in haskell)

