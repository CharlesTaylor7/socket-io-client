const state = require('../state');
const chalk = require('chalk');

function prettyPrint() {
  const { width, height, tiles } = state.map;
  console.log(`${width}x${height}`);

  let tileIndex = 0;
  for (let j = 0; j < height; j++) {
    let row = '|';
    for (let i = 0; i < width; i++) {
      const tile = tiles[tileIndex];
      row += show(tile) + '|'
      tileIndex++;
    }
    console.log(row);
  }
}

function show({ army, terrain, color }) {
  // mountain
  if (terrain === 'mountain') {
    return chalk.grey(' ^ ');
  }
  // fog
  if (terrain === 'fog') {
    return chalk.grey(' - ');
  }
  // fog mountain/city
  if (terrain === 'fog_obstacle') {
    return chalk.grey('^-^');
  }

  // empty
  let style = chalk[color];

  if (terrain === 'general') {
    style = style.bold;
  }

  return style(army.toFixed(0).padStart(3));
}

module.exports = prettyPrint;
