// Read env variables
const { envBotConfig, envGameId, envGameSize } = require('./env');
const botConfig = envBotConfig();
const gameId = envGameId();
const gameSize = envGameSize();

const setUsername = require('./utils/setUsername');
const notifyQuit = require('./utils/notifyQuit');

const state = require('./state');

const Socket = require('socket.io-client');
const socket = Socket('http://botws.generals.io');

const onEvent = socket.onevent;
socket.onevent = function (packet) {
  const { data } = packet;
  const eventName = data[0];
  // Log packets that don't have event handlers
  if (!socket._callbacks.hasOwnProperty(`$${eventName}`)) {
    console.log('Unhandled packet:', JSON.stringify(packet))
  }
  onEvent.call(this, packet);
}

socket.on('disconnect', function() {
  notifyQuit('Disconnected from server.');
});

socket.on('connect', function() {
  console.log('Connected to server.');
  setUsername(socket, botConfig);
  socket.emit('join_private', gameId, botConfig.id);
  const gameUrl = `http://bot.generals.io/games/${encodeURIComponent(gameId)}`
  console.log(`Joined custom game at ${gameUrl}`);
});

// ignore
socket.on('pre_game_start', () => {});
socket.on('notify', () => {});
socket.on('chat_message', () => {});
socket.on('error_set_username', () => {});

socket.on('error_user_id', () => {
  notifyQuit(`User id: ${botConfig.id} is already in a game.`);
});

socket.on('error_banned', (errorMessage) => {
  console.error(errorMessage);
});

socket.on('game_start', function(data) {
  state.gameInfo = data;
  console.log('Game starting!');
});

function leaveGame() {
  socket.emit('leave_game');
}

socket.on('game_won', leaveGame);
socket.on('game_lost', leaveGame);
socket.on('game_over', leaveGame);


socket.on('queue_update', ({ numPlayers }) => {
  if (numPlayers === gameSize) {
    socket.emit('set_force_start', gameId, true);
  }
})

require('hot-module-replacement')({
  // options are optional
  ignore: /node_modules/  // regexp to decide if module should be ignored; also can be a function accepting string and returning true/false
})


require('./strategy/loadStrategy')(socket);
if (module.hot) {
  module.hot.accept('./strategy/loadStrategy', () => {
    require('./strategy/loadStrategy')(socket);
  });
}
