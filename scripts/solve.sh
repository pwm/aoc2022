#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

year=$(date +%Y)
day=$(date +%d)

if [[ $# -eq 2 ]]; then
  cabal run aoc -- solve --year "$1" --day "$2"
elif [[ $# -eq 1 ]]; then
  cabal run aoc -- solve --year "$year" --day "$1"
else
  cabal run aoc -- solve --year "$year" --day "$day"
fi
