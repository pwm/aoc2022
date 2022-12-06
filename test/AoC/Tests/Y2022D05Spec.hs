module AoC.Tests.Y2022D05Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D05
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 05)
    parse
    (solveA, "FZCMJCRHZ")
    (solveB, "JSDHQMZGF")
