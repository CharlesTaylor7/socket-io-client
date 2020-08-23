#/bin/bash

echo 'Not implemented'

exit 1

# ToDo: Use standard git command
# ROOT=$(git root)

yarn
# ToDo: separate build commands for js deps
./scripts/build-js.sh
./scripts/build-css.sh
./scripts/build-js.sh

#  ToDo: Pass build command into shell
./scripts/shell.sh
./scripts/build-hs.sh

# ToDo: Build ghcjs runtime & copy to correct location
