self.addEventListener('install', event => {
  event.waitUntil(
    caches.open('static-assets')
      .then(cache => cache.addAll([
        '/js/static/ghcjs/lib.js',
        '/js/static/ghcjs/rts.js',
        '/js/static/ghcjs/runmain.js',
      ]))
  );
 }
);

const replayStorageRegex = /https:\/\/generalsio-replays-(na|bot)\.s3\.amazonaws\.com\/\w+\.gior/

self.addEventListener('fetch', event => {
  console.log(event.request.url);
  if (replayStorageRegex.test(event.request.url)) {
    console.log("looking in replays cache")
    event.respondWith(
      (async function() {
        const replaysCache = await caches.open('replays')
        let response = await replaysCache.match(event.request)
        if (response) {
          console.log("returning from cache")
          return response
        }
        console.log("performing request from cache")
        response = await fetch(event.request)
        await replaysCache.put(event.request, response);
        return response
      }())
    );
  }
});
