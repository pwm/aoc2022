module AoC (aoc) where

import AoC.Core.ArgParser
import AoC.Core.Fetcher
import AoC.Core.Solver
import AoC.Lib.Prelude
import AoC.Puzzles.Y2022D01 qualified as Y2022D01
import AoC.Puzzles.Y2022D02 qualified as Y2022D02
import AoC.Puzzles.Y2022D03 qualified as Y2022D03
import AoC.Puzzles.Y2022D04 qualified as Y2022D04
import AoC.Puzzles.Y2022D05 qualified as Y2022D05
import AoC.Puzzles.Y2022D06 qualified as Y2022D06
import AoC.Puzzles.Y2022D07 qualified as Y2022D07
import AoC.Puzzles.Y2022D08 qualified as Y2022D08
import AoC.Puzzles.Y2022D09 qualified as Y2022D09
import AoC.Puzzles.Y2022D10 qualified as Y2022D10
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
      ((2022, 04), mkSolverFor Y2022D04.parse Y2022D04.solveA Y2022D04.solveB),
      ((2022, 05), mkSolverFor Y2022D05.parse Y2022D05.solveA Y2022D05.solveB),
      ((2022, 06), mkSolverFor Y2022D06.parse Y2022D06.solveA Y2022D06.solveB),
      ((2022, 07), mkSolverFor Y2022D07.parse Y2022D07.solveA Y2022D07.solveB),
      ((2022, 08), mkSolverFor Y2022D08.parse Y2022D08.solveA Y2022D08.solveB),
      ((2022, 09), mkSolverFor Y2022D09.parse Y2022D09.solveA Y2022D09.solveB),
      ((2022, 10), mkSolverFor Y2022D10.parse Y2022D10.solveA Y2022D10.solveB)
    ]
