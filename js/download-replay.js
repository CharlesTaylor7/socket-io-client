const FetchBase64 = require('fetch-base64-in-browser')
const LZString = require('lz-string')

const fetchReplay = (url) => new FetchBase64(url)
  .fetchAsData()
  .then(base64 => {
    var obj = JSON.parse(
      LZString.decompressFromUint8Array(
        new Uint8Array(base64)
      )
    );
    console.log(obj)
  })
  .catch(console.error)

module.exports = fetchReplay
