module AoC.Tests.Y2022D03Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D03
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 03)
    parse
    (solveA, 7_908)
    (solveB, 2_838)
