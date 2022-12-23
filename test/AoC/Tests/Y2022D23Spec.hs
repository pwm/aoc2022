module AoC.Tests.Y2022D23Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D23
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 23)
    parse
    (solveA, 3_931)
    (solveB, 944)
