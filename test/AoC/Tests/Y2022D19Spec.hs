module AoC.Tests.Y2022D19Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D19
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 19)
    parse
    (solveA, 9)
    (solveB, ())
