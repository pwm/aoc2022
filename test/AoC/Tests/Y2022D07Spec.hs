module AoC.Tests.Y2022D07Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D07
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 07)
    parse
    (solveA, 1_723_892)
    (solveB, 8_474_158)
