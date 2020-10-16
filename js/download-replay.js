#! /usr/bin/env node

// arg: url of a replay
const [url] = process.argv.slice(2);

const fetch = require('fetch-base64');
const LZString = require('lz-string');

const base64Prefix = 'data:application/octet-stream;base64,'

fetch.auto(url)
  .then(data => {
    const [base64] = data
    const decompressed = LZString.decompressFromBase64(base64);
    process.stdout.write(decompressed);
  })
  .catch(console.error)

