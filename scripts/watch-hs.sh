#!/bin/bash
ROOT=$(git root)
git ls-files "$ROOT/hs/" | entr -rc "$ROOT/scripts/build-hs.sh"

