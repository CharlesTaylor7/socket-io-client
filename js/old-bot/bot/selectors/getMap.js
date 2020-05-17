const state = require('../state');
const colors = require('../utils/colors');

const terrainMap = {
  [-1]: 'open',
  [-2]: 'mountain',
  [-3]: 'fog',
  [-4]: 'fog_obstacle',
}

function getTerrain(k) {
  const { rawmap: map, cities, generals } = state;
  const width = map[0];
  const height = map[1];
  const size = height * width;
  const tileIndex = k + 2;
  const army = map[k + 2];
  const terrainIndex = map[k + 2 + size];

  const color = colors[terrainIndex] || 'grey';
  const terrain = cities.includes(tileIndex)
    ? 'city'
    : generals.includes(k)
      ? 'general'
      : terrainMap[terrainIndex] || 'open';

  return {
    color,
    terrain,
    army,
  }
}

function getMap() {
  const { rawmap: map } = state;
  const width = map[0];
  const height = map[1];
  const size = height * width;

  const tiles = Array.from(
    { length: size },
    (_, k) => getTerrain(k)
  );

  return { height, width, tiles };
}

module.exports = getMap;
