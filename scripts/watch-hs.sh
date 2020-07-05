#!/bin/bash
ROOT=$(git root)
git ls-files "$ROOT/hs/" | entr -r "$ROOT/scripts/build-hs.sh"

