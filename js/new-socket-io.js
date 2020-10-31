const Socket = require('socket.io-client');
const url = process.argv[2]
const socket = Socket(url);

socket.on('connect', function() {
  process.stdout.write('connect')
});

socket.on('disconnect', function() {
  process.stdout.write('disconnect')
  process.exit(1)
});

socket.onevent = function (packet) {
  const { data } = packet;
  process.stdout.write(JSON.stringify(data))
}

process.stdin.on('data', function(data) {
  socket.emit(...JSON.parse(data.toString()))
});