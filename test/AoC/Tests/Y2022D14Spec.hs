module AoC.Tests.Y2022D14Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D14
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 14)
    parse
    (solveA, 592)
    (solveB, 30_367)
