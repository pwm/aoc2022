#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

hpack --force
cabal build --enable-tests
