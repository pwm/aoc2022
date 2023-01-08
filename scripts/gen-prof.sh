#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

[[ -f aoc.svg ]] && cp aoc{,.old}.svg
ghc-prof-flamegraph aoc.prof

[[ -f aoc.prof.html ]] && cp aoc.prof{,.old}.html
profiteur aoc.prof

[[ -f aoc.profiterole.txt ]] && cp aoc.profiterole{,.old}.txt
[[ -f aoc.profiterole.html ]] && cp aoc.profiterole{,.old}.html
profiterole aoc.prof
