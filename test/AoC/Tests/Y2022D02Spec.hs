module AoC.Tests.Y2022D02Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D02
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 02)
    parse
    (solveA, 11_603)
    (solveB, 12_725)
