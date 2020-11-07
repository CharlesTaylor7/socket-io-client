const Socket = require('socket.io-client')
const url = process.argv[2]
const socket = Socket(url)

function sendEvent(data) {
  process.stdout.write(JSON.stringify(data))
  process.stdout.write('\n')
}

// forward socket.io events to parent process
socket.on('connect', function() {
  process.stdout.write('connect\n')
});

// forward exit process
socket.on('disconnect', function() {
  // node makes use of exit codes 1 through 12
  process.exit(125)
});

// forward other events
 const onEvent = socket.onevent;
 socket.onevent = function (packet) {
  const { data } = packet
  sendEvent(data)
  onEvent.call(this, packet)
}

// forward input from parent process to socket.io server
const readline = require('readline').createInterface({input: process.stdin})
readline.on('line', data => {
  const string = data.toString()
  const args = JSON.parse(string)

  socket.emit(...args)
})
