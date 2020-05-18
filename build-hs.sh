cd hs/

if cabal build generals-app; then
  public_build="../public/js/ghcjs/build"
  build_output="dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/generals-0.0.0.0/x/generals-app/build/generals-app/generals-app.jsexe"
  mkdir -p "$public_build"
  # cp "$build_output"/lib.js "$public_build"
  cp "$build_output"/out.js "$public_build"
fi
