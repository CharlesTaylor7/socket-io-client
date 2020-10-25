

module.exports = function patch(array, diff) {
  let diffIndex = 0;
  let arrIndex = 0;
  while (diffIndex < diff.length - 1) {
    arrIndex += diff[diffIndex];
    diffIndex++;
    const toChange = diff[diffIndex];
    diffIndex++;
    for (let i = 0; i < toChange; i++) {
      array[arrIndex] = diff[diffIndex];
      arrIndex++;
      diffIndex++;
    }
  }
}
