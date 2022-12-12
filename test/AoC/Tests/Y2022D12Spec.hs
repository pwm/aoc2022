module AoC.Tests.Y2022D12Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D12
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 12)
    parse
    (solveA, 472)
    (solveB, 465)
