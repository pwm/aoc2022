module AoC.Tests.Y2022D13Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D13
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 13)
    parse
    (solveA, 5_366)
    (solveB, 23_391)
