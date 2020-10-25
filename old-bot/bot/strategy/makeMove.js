const state = require('../state');

function getEnd(direction) {
  switch (direction) {
    case 'left':
      return x => x - 1;
    case 'right':
      return x => x + 1;
    case 'down':
      return x => x + state.map.width;
    case 'up':
      return x => x - state.map.width;
  }
}

function makeMove(socket, direction) {
  const location = state.generals[state.gameInfo.playerIndex];
  const end = getEnd(direction)(location);
  console.log("direction", direction, "start", location, "end", end);
  socket.emit('attack', location, end, true);
}

module.exports = makeMove;
