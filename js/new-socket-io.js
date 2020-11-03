const Socket = require('socket.io-client')
const url = process.argv[2]
const socket = Socket(url)

function sendEvent(data, options = { final: false }) {
  process.stdout.write(JSON.stringify(data))
  if (!options.final) {
    process.stdout.write("\n")
  }
}

// forward socket.io events to parent process
socket.on('connect', function() {
  sendEvent(['connect'])
});

// forward diconnect event & exit process
socket.on('disconnect', function() {
  sendEvent(['disconnect'], { final: true})
  process.exit(1)
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
  sendEvent([string])
  const args = JSON.parse(string)

  socket.emit(...args)
})
