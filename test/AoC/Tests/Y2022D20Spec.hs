module AoC.Tests.Y2022D20Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D20
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 20)
    parse
    (solveA, ())
    (solveB, ())
