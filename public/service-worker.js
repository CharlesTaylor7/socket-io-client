self.addEventListener('install', (e) => {
  e.waitUntil(
    caches.open('airhorner').then((cache) => {
      return cache.addAll([
        '/js/static/ghcjs/lib.js',
        '/js/static/ghcjs/rts.js',
        '/js/static/ghcjs/runmain.js',
      ]);
    })
  );
 }
);

 self.addEventListener('fetch', function(event) {
  console.log(event.request.url);
 });
