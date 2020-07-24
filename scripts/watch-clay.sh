#!/bin/bash

ROOT=$(git root)
git ls-files "$ROOT/css/" | entr -rc "$ROOT/scripts/build-clay.sh"

