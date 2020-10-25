const fs = require('fs');

module.exports = function setUsername(socket, botConfig) {
  if (botConfig.registered) return;

  socket.emit('set_username', botConfig.id, botConfig.name);

  const json = require('../.env.json');
  json[botConfig.nick].registered = true;
  fs.writeFileSync('../.env.json', JSON.stringify(json));
}
