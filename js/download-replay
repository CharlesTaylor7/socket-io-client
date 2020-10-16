#! /usr/bin/env node

const [url] = process.argv.slice(2);

const FetchBase64 = require('fetch-base64');
const LZString = require('lz-string');

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

    process.stdout.write(decompressed);
  })
  .catch(console.error)

