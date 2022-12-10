module AoC.Puzzles.Y2022D08 where

import AoC.Lib.Grid
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe (GridOf Int)
parse = parseGrid charToDigit

solveA :: GridOf Int -> Int
solveA g = length . filter (== True) . Map.elems . Map.mapWithKey (canSeeEdge g) $ g

solveB :: GridOf Int -> Int
solveB g = maximum . Map.elems . Map.mapWithKey (visibleTrees g) $ g

canSeeEdge :: GridOf Int -> Pos -> Int -> Bool
canSeeEdge grid p v =
  any (\p2path -> fst $ visiblePath grid (p2path p) v) (lurd grid)

visibleTrees :: GridOf Int -> Pos -> Int -> Int
visibleTrees grid p v =
  product $ map (\p2path -> snd $ visiblePath grid (p2path p) v) (lurd grid)

visiblePath :: GridOf Int -> [Pos] -> Int -> (Bool, Int)
visiblePath = go 0
  where
    go :: Int -> GridOf Int -> [Pos] -> Int -> (Bool, Int)
    go c _ [] _ = (True, c)
    go c grid (p : ps) v
      | Just v' <- grid !? p, v' >= v = (False, c + 1)
      | otherwise = go (c + 1) grid ps v

lurd :: GridOf Int -> [Pos -> [Pos]]
lurd grid = [l, u, r, d]
  where
    l, u, r, d :: Pos -> [Pos]
    l (x, y) = [(x, yl) | yl <- [y - 1, y - 2 .. 0]]
    u (x, y) = [(xu, y) | xu <- [x - 1, x - 2 .. 0]]
    r (x, y) = [(x, yr) | yr <- [y + 1 .. (isqrt (Map.size grid) - 1)]]
    d (x, y) = [(xd, y) | xd <- [x + 1 .. (isqrt (Map.size grid) - 1)]]
    isqrt :: Int -> Int
    isqrt = floor @Double . sqrt . fromIntegral
