self.addEventListener('install', event =>
  event.waitUntil(
    caches.open('static-assets')
      .then(cache => cache.addAll([
        '/js/static/ghcjs/lib.js',
        '/js/static/ghcjs/rts.js',
        '/js/static/ghcjs/runmain.js',
      ]))
  )
);

const replayStorageRegex = /https:\/\/generalsio-replays-(na|bot)\.s3\.amazonaws\.com\/\w+\.gior/

self.addEventListener('fetch', event => {
  if (replayStorageRegex.test(event.request.url)) {
    event.respondWith(
      (async function() {
        const replaysCache = await caches.open('replays')
        let response = await replaysCache.match(event.request)
        if (response) {
          return response
        }
        response = await fetch(event.request)
        await replaysCache.put(event.request, response);
        return response
      }())
    );
  }
});
