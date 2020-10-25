const changeHandler = require('./changeHandler');
const newStrategy = require('./strategy');

function loadStrategy(socket) {
  console.log('reload');
  const strategy = newStrategy(socket);
  changeHandler(socket, 'ping_tile', strategy.onPingTile);
  changeHandler(socket, 'game_update', strategy.onGameUpdate);
}

module.exports = loadStrategy;
