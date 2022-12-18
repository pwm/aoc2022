module AoC.Tests.Y2022D18Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D18
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 18)
    parse
    (solveA, 4_308)
    (solveB, ())
