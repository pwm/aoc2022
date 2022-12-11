module AoC.Tests.Y2022D11Spec (spec) where

import AoC.Core.Date
import AoC.Puzzles.Y2022D11
import AoC.Tester
import Test.Hspec

spec :: Spec
spec =
  tester
    (mkDate 2022 11)
    parse
    (solveA, 110_264)
    (solveB, 23_612_457_316)
