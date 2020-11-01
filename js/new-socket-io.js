const Socket = require('socket.io-client')
const url = process.argv[2]
const socket = Socket(url)

function sendEvent(data) {
  process.stdout.write(JSON.stringify(data))
}

// forward socket.io events to parent process
socket.on('connect', function() {
  sendEvent({'type': 'connect'})
});

// forward diconnect event & exit process
socket.on('disconnect', function() {
  sendEvent({'type': 'disconnect'})
  process.exit(1)
});

// forward other events
onEventOriginal = socket.onevent
socket.onevent = function (packet) {
  const { data } = packet
  sendEvent(data)

  onEventOriginal(packet)
}

// forward input from parent process to socket.io server
process.stdin.on('data', function(data) {
  args = JSON.parse(data.toString())
  socket.emit(...args)
})
