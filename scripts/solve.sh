#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

year=$(date +%Y)
day=$(date +%d)

prof_flag=''
if [[ "$(rg --count "^profiling:\s*True" cabal.project)" == 1 ]]; then
  printf 'Profiled build!\n\n'
  prof_flag='-p'
  [[ -f aoc.prof ]] && cp aoc.prof aoc.prof.old
fi

if [[ $# -eq 2 ]]; then
  cabal run aoc -- +RTS $prof_flag -T -RTS solve --year "$1" --day "$2"
elif [[ $# -eq 1 ]]; then
  cabal run aoc -- +RTS $prof_flag -RTS solve --year "$year" --day "$1"
else
  cabal run aoc -- +RTS $prof_flag -RTS solve --year "$year" --day "$day"
fi

# Generate various profiling graphs
if [[ "$(rg --count "^profiling:\s*True" cabal.project)" == 1 ]]; then
  printf '\nGenerating profiling graphs ...\n'

  [[ -f aoc.svg ]] && cp aoc.svg aoc.svg.old
  ghc-prof-flamegraph aoc.prof

  [[ -f aoc.prof.html ]] && cp aoc.prof.html aoc.prof.html.old
  profiteur aoc.prof

  [[ -f aoc.profiterole.txt ]] && cp aoc.profiterole.txt aoc.profiterole.txt.old
  [[ -f aoc.profiterole.html ]] && cp aoc.profiterole.html aoc.profiterole.html.old
  profiterole aoc.prof

  printf 'Done.'
fi
