# generals-bot

## Install for development

- clone repo
- run `yarn` to install node dependencies
- clone reflex-platform to your computer and add reflex-platform/scripts to your PATH variable.

## Running the dev server

- open a WSL terminal. Run `./shell.sh`. Then inside the virtual reflex shell, run `yarn watch:hs`.
- open any terminal. Run `yarn watch:server`.
- open another terminal. Run `yarn watch:js`.

Congrats, you should have built all the sources and have a server running at localhost:8000.

## To do

### Replays
- View replays from global perspective
- view replays from player perspectives

### Bot
- Port old bot client to Haskell
- Hot reloading of bot strategies (strategies in haskell)

### Ops
- single "start" script to launch 3 watch windows (watch haskell, js, server)
