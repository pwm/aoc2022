module AoC.Puzzles.Y2022D04 where

import AoC.Lib.Prelude

parse :: String -> Maybe [((Int, Int), (Int, Int))]
parse = traverse (bitraverse toPair toPair <=< toPairs) . lines
  where
    toPairs = l2p . splitOn ","
    toPair = l2p <=< traverse stringToInt . splitOn "-"

solveA :: [((Int, Int), (Int, Int))] -> Int
solveA = length . filter (== True) . map contains

solveB :: [((Int, Int), (Int, Int))] -> Int
solveB = length . filter (== True) . map overlap

contains :: ((Int, Int), (Int, Int)) -> Bool
contains ((a, b), (x, y)) = a <= x && b >= y || x <= a && y >= b

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (x, y)) = if b <= y then b >= x else a <= y
