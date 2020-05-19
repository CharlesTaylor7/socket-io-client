cd hs/

if cabal build generals-app; then

  # directories
  public_root="../public"
  public_build="${public_root}/js/ghcjs/build"
  build_output="dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/generals-0.0.0.0/x/generals-app/build/generals-app/generals-app.jsexe"

  # generate unique identifier for each build
  version=$(uuidgen)

  # echo it to the terminal
  echo "version: ${version}"

  # copy build output to public directory
  mkdir -p "$public_build"
  cp "${build_output}"/out.js "${public_build}"

  # copy version to public directory
  echo "${version}" > "${public_root}/version.txt"
fi
