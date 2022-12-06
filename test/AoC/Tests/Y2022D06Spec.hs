module AoC.Tests.Y2022D06Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D06
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 06)
    parse
    (solveA, 1_833)
    (solveB, 3_425)
