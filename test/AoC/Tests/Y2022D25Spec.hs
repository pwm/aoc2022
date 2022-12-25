module AoC.Tests.Y2022D25Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D25
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 25)
    parse
    (solveA, "2-=0-=-2=111=220=100")
    (solveB, ())
