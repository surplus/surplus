#!/usr/bin/env bash
set -xeuo pipefail

cd "$(dirname "$0")"
rm -rf pkg
wasm-pack build --release --no-pack -d pkg/browser -t web
wasm-pack build --release --no-pack -d pkg/node -t nodejs
