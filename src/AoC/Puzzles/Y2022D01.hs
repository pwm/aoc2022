module AoC.Puzzles.Y2022D01 where

import AoC.Prelude

parse :: String -> Maybe [[Int]]
parse = stringBlocksToInts

solveA :: [[Int]] -> Int
solveA = headOr 0 . take 1 . rsort . map sum

solveB :: [[Int]] -> Int
solveB = sum . take 3 . rsort . map sum
