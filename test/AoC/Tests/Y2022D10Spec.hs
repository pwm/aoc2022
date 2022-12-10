module AoC.Tests.Y2022D10Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D10
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 10)
    parse
    (solveA, 14_920)
    (solveB, "BUCACBUZ")
