# socket-io-client
Socket.io client for Haskell.

Right now this is just a light wrapper around the npm package [socket.io-client](https://www.npmjs.com/package/socket.io-client).

Eventually, I may reimplement the Socket.io protocol in Haskell.

# generals-io-client
Will eventually break this into 1 or 2 separate packages.
1 for datatypes & parsers for the core data types. low dependency & low opinions library. `generalsIO-types`

Another `generalsIO-client`, will be very opinionated. May lock into a lens library. Will lock into mtl, and pipes. Will provide the kitchen sink for joining games & teams, and writing composable bot behaviors.


## Features:
- parsers & renderers for the generals.io protocol
- parse and run game replays
- type safe interface to the commands & events in the generals.io protocol
- modular composable bot strategies


## TODO:
- use jsonifier for faster command rendering
