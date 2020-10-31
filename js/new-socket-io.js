const Socket = require('socket.io-client');
const socket = Socket('http://botws.generals.io');

const onEvent = socket.onevent;
socket.onevent = function (packet) {
  const { data } = packet;
  const eventName = data[0];
  // Log packets that don't have event handlers
  if (!socket._callbacks.hasOwnProperty(`$${eventName}`)) {
    console.log("data", JSON.stringify(data))
    console.log("packet", JSON.stringify(packet))
  }
  onEvent.call(this, packet);
}

socket.on('disconnect', function() {
  notifyQuit('Disconnected from server.');
});

socket.on('connect', function() {
  console.log('Connected to server.');
});

