#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

run_tests() {
  # eg. scripts/test.sh --match "Day01" --match "Day02"
  arr=()
  for i in "$@"; do
    arr+=("--test-option" "$i")
  done

  cabal --ghc-options -Wwarn test "${arr[@]}"
}

time run_tests "$@"
