module AoC.Tests.Y2022D16Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D16
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 16)
    parse
    (solveA, 1_559)
    (solveB, ())
