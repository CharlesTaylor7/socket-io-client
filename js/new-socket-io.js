const Socket = require('socket.io-client');
const url = process.argv[2]
const socket = Socket(url);

// forward socket.io events to parent process
socket.on('connect', function() {
  process.stdout.write('"connect"')
  process.stdout.write('\n')
});

// forward diconnect event & exit process
socket.on('disconnect', function() {
  process.stdout.write('"disconnect"')
  process.exit(1)
});

// forward other events
socket.onevent = function (packet) {
  const { data } = packet;
  process.stdout.write(JSON.stringify(data))
  process.stdout.write("\n")
}

// forward input from parent process to socket.io server
process.stdin.on('data', function(data) {
  args = JSON.parse(data.toString())
  socket.emit(...args)
});
