# generals-bot

## Install for development
- clone repo
- init submodules
- run `yarn` to install node dependencies
- run `./scripts/shell.sh` this step will take a while the first time.

## Running the dev server
- open a WSL terminal. Run `./scripts/shell.sh`. Then inside the virtual reflex shell, run `./scripts/watch-hs.sh`
- open any terminal. Run `yarn watch:server`.
- open another terminal. Run `yarn watch:js`.

Congrats, you should have built all the sources and have a server running at localhost:8000.

## To do
### Ops
- remove dependencies on js ffi and external scripts
- don't rely on browser cache or service worker
- get jsaddle-warp working

### Replays
- View replays from global perspective
- view replays from player perspectives
- Cache replays at the app level?
- fix bug in simulate that causes crashing
- fix css
- ui for importing a replay? paste a url in an input?
- loading animation while replay loads?
- handle afks

### Bot
- Port old bot client to Haskell
- Hot reloading of bot strategies (strategies in haskell)

