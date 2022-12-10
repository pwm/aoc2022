#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

year=$(date +%Y)
day=$(date +%d)
if [[ $# -eq 2 ]]; then
  year="$1"
  day="$2"
elif [[ $# -eq 1 ]]; then
  year=$(date +%Y)
  day="$1"
fi

# Note: This will fail if the input file is already present
# and thus we will not execute the cp/sed commands below
cabal run aoc -- fetch --year "$year" --day "$day"

# Create source file template

src_content=$(
  cat <<-EOF
module AoC.Puzzles.Y${year}D${day} where

import AoC.Lib.Prelude

parse :: String -> Maybe ()
parse _ = Just ()

solveA :: a -> ()
solveA _ = ()

solveB :: a -> ()
solveB _ = ()
EOF
)

src_file=src/AoC/Puzzles/Y"$year"D"$day".hs
echo "$src_content" >"$src_file"
echo "Created source file $src_file"

# Create test file template

test_content=$(
  cat <<-EOF
module AoC.Tests.Y${year}D${day}Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y${year}D${day}
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate ${year} ${day})
    parse
    (solveA, ())
    (solveB, ())
EOF
)

test_file=test/AoC/Tests/Y"$year"D"$day"Spec.hs
echo "$test_content" >"$test_file"
echo "Created test file $test_file"

# Regenerate cabal file from package.yaml

hpack --force
echo "Ran hpack"
