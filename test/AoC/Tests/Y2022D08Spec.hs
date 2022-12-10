module AoC.Tests.Y2022D08Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D08
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 08)
    parse
    (solveA, 1_711)
    (solveB, 301_392)
