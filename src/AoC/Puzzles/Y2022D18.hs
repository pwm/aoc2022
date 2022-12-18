module AoC.Puzzles.Y2022D18 where

import AoC.Lib.Prelude

parse :: String -> Maybe [P]
parse = traverse l2p3 <=< traverse (stringToIntsSepBy ",") . lines

solveA :: [P] -> Int
solveA ps = allSides ps - connectedSides ps

solveB :: a -> ()
solveB _ = ()

type P = (Int, Int, Int)

internalSides :: [P] -> Int
internalSides = const 0

connectedSides :: [P] -> Int
connectedSides = sum . map touching . fromJust . traverse l2p . pick 2

allSides :: [P] -> Int
allSides = (* 6) . length

touching :: (P, P) -> Int
touching ((a, b, c), (x, y, z))
  | (x - 1 == a || x + 1 == a) && b == y && c == z = 2
  | a == x && (y - 1 == b || y + 1 == b) && c == z = 2
  | a == x && b == y && (z - 1 == c || z + 1 == c) = 2
  | otherwise = 0
