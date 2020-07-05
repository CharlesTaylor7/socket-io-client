cd hs/

if cabal build generals-app; then

  # directories
  public_root="../public"
  public_build="${public_root}/js/build/ghcjs"
  build_output="dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/generals-0.0.0.0/x/generals-app/build/generals-app/generals-app.jsexe"


  # copy build output to public directory
  mkdir -p "$public_build"
  cp "${build_output}"/out.js "${public_build}"

  echo "Done"
fi
