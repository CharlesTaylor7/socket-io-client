const makeSafe = require('../utils/makeSafe');

module.exports = function(socket, eventName, handler) {
  socket.removeListener(eventName);
  socket.on(eventName, makeSafe(handler));
}
