## (ﾉ ◕ ヮ ◕)ﾉ\*:･ﾟ ✧ [Advent of Code 2022](https://adventofcode.com/2022) (◕‿◕✿)

## Setup

We'll need 2 env vars:

1. `AOC_SESSION` will hold your personal AoC session cookie
2. `AOC_INPUT_PATH` is the path to the `input` dir within this repo, eg. `~/code/aoc2022/input`

```
echo 'export AOC_SESSION=<my-aoc-session>' >> .envrc.private
echo 'export AOC_INPUT_PATH=<my-aoc-input-path>' >> .envrc.private
direnv allow .
```

## Use

```
scripts/comp.sh
scripts/solve.sh # current date
scripts/solve.sh 01 # specific day
scripts/solve.sh 2022 01 # specific date
```

Note: `optimization` in `cabal.project` is set to 0 for fast dev UX. Setting it to 1 can significantly speed up solvers.

## Test

```
scripts/test.sh
```

## Nix

```
build
aoc fetch --year 2022 --day 01
aoc solve --year 2022 --day 01
```

## Dev

The project is using [Nixkell](https://github.com/pwm/nixkell)

`mkday.sh` will fetch the input file and generate a template module and spec for it.

```
scripts/mkday.sh # current date
scripts/mkday.sh 01 # current year, specific day
scripts/mkday.sh 2022 01 # specific date
```

Formatting, Linting, Checking, etc...

```
treefmt
hlint .
scripts/shellcheck.sh
```

To make CI push to Cachix

Configure `CACHIX_SIGNING_KEY` as a repository secret (see: https://nix.dev/tutorials/continuous-integration-github-actions)
