const state = require('../state');
const patch = require('../utils/patch');
const prettyPrint = require('../utils/prettyPrint');
const makeMove = require('./makeMove');
const getMap = require('../selectors/getMap');

module.exports = socket => ({
  onGameUpdate: function (data) {
    const { map_diff, cities_diff, ...rest } = data;
    state.rawmap = state.rawmap || [];
    patch(state.rawmap, map_diff);

    state.cities = state.cities || [];
    patch(state.cities, cities_diff);
    Object.assign(state, rest);
    state.map = getMap();

    prettyPrint();
  },
});
