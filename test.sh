#/bin/bash

set -e

cargo build --release

for f in test/*.ml; do
    echo $f
    ./target/release/mc $f >/dev/null
done
