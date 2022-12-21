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
import AoC.Puzzles.Y2022D11 qualified as Y2022D11
import AoC.Puzzles.Y2022D12 qualified as Y2022D12
import AoC.Puzzles.Y2022D13 qualified as Y2022D13
import AoC.Puzzles.Y2022D14 qualified as Y2022D14
import AoC.Puzzles.Y2022D15 qualified as Y2022D15
import AoC.Puzzles.Y2022D16 qualified as Y2022D16
import AoC.Puzzles.Y2022D17 qualified as Y2022D17
import AoC.Puzzles.Y2022D18 qualified as Y2022D18
import AoC.Puzzles.Y2022D19 qualified as Y2022D19
import AoC.Puzzles.Y2022D20 qualified as Y2022D20
import AoC.Puzzles.Y2022D21 qualified as Y2022D21
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
      ((2022, 10), mkSolverFor Y2022D10.parse Y2022D10.solveA Y2022D10.solveB),
      ((2022, 11), mkSolverFor Y2022D11.parse Y2022D11.solveA Y2022D11.solveB),
      ((2022, 12), mkSolverFor Y2022D12.parse Y2022D12.solveA Y2022D12.solveB),
      ((2022, 13), mkSolverFor Y2022D13.parse Y2022D13.solveA Y2022D13.solveB),
      ((2022, 14), mkSolverFor Y2022D14.parse Y2022D14.solveA Y2022D14.solveB),
      ((2022, 15), mkSolverFor Y2022D15.parse Y2022D15.solveA Y2022D15.solveB),
      ((2022, 16), mkSolverFor Y2022D16.parse Y2022D16.solveA Y2022D16.solveB),
      ((2022, 17), mkSolverFor Y2022D17.parse Y2022D17.solveA Y2022D17.solveB),
      ((2022, 18), mkSolverFor Y2022D18.parse Y2022D18.solveA Y2022D18.solveB),
      ((2022, 19), mkSolverFor Y2022D19.parse Y2022D19.solveA Y2022D19.solveB),
      ((2022, 20), mkSolverFor Y2022D20.parse Y2022D20.solveA Y2022D20.solveB),
      ((2022, 21), mkSolverFor Y2022D21.parse Y2022D21.solveA Y2022D21.solveB)
    ]
