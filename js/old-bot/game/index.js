const args = process.argv.slice(2);
const gameSize = args[0];
const botNicks = args.slice(1);
const gameName = require('uuid').v4();
const { spawn } = require('child_process');

async function main() {
  for (let nick of botNicks) {
    spawn(
      'node',
      ['bot', nick, gameName, gameSize],
      {
        cwd: process.cwd(),
        detached: true,
        stdio: "inherit",
        shell: true
      }
    );
    await new Promise(resolve => setTimeout(resolve, 2000));
  }
}

main().catch(console.error);
