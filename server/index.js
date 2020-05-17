require("live-server").start({
  port: 8080,
  host: "0.0.0.0",
  root: "public",
  open: false,
  file: "index.html",
  wait: 1000,
  logLevel: 2,
});
