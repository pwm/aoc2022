module AoC.Tests.Y2022D22Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D22
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 22)
    parse
    (solveA, 122_082)
    (solveB, ())
