module AoC.Tests.Y2022D09Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D09
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 09)
    parse
    (solveA, 6_354)
    (solveB, 2_651)
