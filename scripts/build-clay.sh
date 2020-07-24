#!/bin/bash

echo "Regenerating css"
ROOT=$(git root)

OUTPUT_DIR="$ROOT/public/build/css/"
mkdir -p $OUTPUT_DIR
touch "$OUTPUT_DIR/clay-gen.css"

"$ROOT/css/Main.hs" > "$OUTPUT_DIR/clay-gen.css"

echo "Done"
