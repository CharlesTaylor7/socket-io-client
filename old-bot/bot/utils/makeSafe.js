module.exports = (fn) => {
  return function (...args) {
    try {
      fn(...args);
    } catch (e) {
      console.error(e);
    }
  }
}
