module AoC.Tests.Y2022D17Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D17
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 17)
    parse
    (solveA, ())
    (solveB, ())
