require("live-server").start({
  port: 8000,
  host: "0.0.0.0",
  root: "public",
  open: false,
  file: "index.html",
  wait: 40,
  logLevel: 2,
  watch: 'public/',
  ignore: 'replays/'
});
