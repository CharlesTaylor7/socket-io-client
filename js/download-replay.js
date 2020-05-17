const FetchBase64 = require('fetch-base64-in-browser')
const LZString = require('lz-string')

const base64Prefix = "data:application/octet-stream;base64,"

const downloadReplay = (url) => new FetchBase64(url)
  .fetchAsData()
  .then(body => {
    const bodyPrefix = body.substring(0, base64Prefix.length);
    if (bodyPrefix !== base64Prefix) {
      throw Error("Body not base 64")
    }

    const base64 = body.substring(bodyPrefix.length);
    const decompressed = LZString.decompressFromBase64(base64);
    return decompressed;
  })
  .catch(console.error)

module.exports = downloadReplay;
