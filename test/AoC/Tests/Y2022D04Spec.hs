module AoC.Tests.Y2022D04Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D04
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 04)
    parse
    (solveA, 413)
    (solveB, 806)
