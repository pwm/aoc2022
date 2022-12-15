module AoC.Tests.Y2022D15Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D15
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 15)
    parse
    (solveA, 5_256_611)
    (solveB, ())
