module AoC.Tests.Y2022D24Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D24
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 24)
    parse
    (solveA, ())
    (solveB, ())
