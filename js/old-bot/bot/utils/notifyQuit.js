module.exports = function(message) {
  console.error(message)
  require('readline-sync').question('Press enter to close window');
  process.exit(1);
}
