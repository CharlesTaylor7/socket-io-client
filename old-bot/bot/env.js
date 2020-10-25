var args = process.argv.slice(2);
const notifyQuit = require('./utils/notifyQuit');

module.exports = {
  envBotConfig: function () {
    if (args[0] === undefined) {
      notifyQuit("Please provide a bot config");
    }
    const botConfig = require('../.env.json')[args[0]];
    botConfig.nick = args[0];
    return botConfig;
  },
  envGameId: function () {
    if (args[1] === undefined) {
      notifyQuit("Please provide a game slug");
    }
    return args[1];
  },
  envGameSize: function () {
    if (args[2] === undefined) {
      notifyQuit("Please provide an expected game size");
    }
    var number = parseInt(args[2]);
    if (number < 2 || number > 8) {
      notifyQuit("Please input a game size between 2 & 8.")
    }
    return parseInt(args[2]);
  },
}
