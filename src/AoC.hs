module AoC (aoc) where

import AoC.Core.ArgParser
import AoC.Core.Fetcher
import AoC.Core.Solver
import AoC.Prelude
import AoC.Puzzles.Y2022D01 qualified as Y2022D01
import Data.Map.Strict qualified as Map

aoc :: IO ()
aoc =
  execParser opts >>= \case
    Fetch date -> fetchPuzzle date
    Solve date -> solvePuzzle solutions date

solutions :: Solutions
solutions =
  Map.fromList
    [ ((2022, 01), mkSolverFor Y2022D01.parse Y2022D01.solveA Y2022D01.solveB)
    ]
