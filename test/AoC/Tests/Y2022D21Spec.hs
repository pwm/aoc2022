module AoC.Tests.Y2022D21Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D21
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 21)
    parse
    (solveA, 54_703_080_378_102)
    (solveB, 3_952_673_930_912)
