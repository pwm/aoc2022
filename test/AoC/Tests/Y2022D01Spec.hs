module AoC.Tests.Y2022D01Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D01
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 01)
    parse
    (solveA, 75_501)
    (solveB, 215_594)
