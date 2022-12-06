module AoC (aoc) where

import AoC.Core.ArgParser
import AoC.Core.Fetcher
import AoC.Core.Solver
import AoC.Prelude
import AoC.Puzzles.Y2022D01 qualified as Y2022D01
import AoC.Puzzles.Y2022D02 qualified as Y2022D02
import AoC.Puzzles.Y2022D03 qualified as Y2022D03
import AoC.Puzzles.Y2022D04 qualified as Y2022D04
import Data.Map.Strict qualified as Map

aoc :: IO ()
aoc =
  execParser opts >>= \case
    Fetch date -> fetchPuzzle date
    Solve date -> solvePuzzle solutions date

solutions :: Solutions
solutions =
  Map.fromList
    [ ((2022, 01), mkSolverFor Y2022D01.parse Y2022D01.solveA Y2022D01.solveB),
      ((2022, 02), mkSolverFor Y2022D02.parse Y2022D02.solveA Y2022D02.solveB),
      ((2022, 03), mkSolverFor Y2022D03.parse Y2022D03.solveA Y2022D03.solveB),
      ((2022, 04), mkSolverFor Y2022D04.parse Y2022D04.solveA Y2022D04.solveB)
    ]
