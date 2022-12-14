module AoC.Puzzles.Y2022D14 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (pad, pps)
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Cave
parse = fmap toCave . parseMaybe linesP

solveA :: Cave -> Int
solveA = solve

solveB :: Cave -> Int
solveB = solve . addFloor

solve :: Cave -> Int
solve = length . filter (== Sand) . Map.elems . fixpoint updateCave

type Cave = GridOf Cell

data Cell = Wall | Sand deriving stock (Show, Eq)

updateCave :: Cave -> Cave
updateCave cave
  | p <- fallSand cave (0, 500) =
      if fst p > maxX cave then cave else Map.insert p Sand cave

fallSand :: Cave -> Pos -> Pos
fallSand cave p
  | p' <- dropSand cave p =
      if p' == p || fst p' > maxX cave then p' else fallSand cave p'

dropSand :: Cave -> Pos -> Pos
dropSand cave p
  | pS <- step p stepS, Map.notMember pS cave = pS
  | pSW <- step p stepSW, Map.notMember pSW cave = pSW
  | pSE <- step p stepSE, Map.notMember pSE cave = pSE
  | otherwise = p

addFloor :: Cave -> Cave
addFloor cave = Map.union cave (Map.fromList (zip (calcFloor cave) (repeat Wall)))
  where
    calcFloor :: Cave -> [Pos]
    calcFloor c =
      let pad = maxX c - minX c
       in [(maxX c + 2, y) | y <- [minY c - pad .. maxY c + pad]]

minX, minY, maxX, maxY :: Cave -> Int
minX = fst . fst . Map.findMin
minY = minX . Map.fromList . map (first swap) . Map.toList
maxX = fst . fst . Map.findMax
maxY = maxX . Map.fromList . map (first swap) . Map.toList

toCave :: [[Pos]] -> Cave
toCave pps = Map.fromList $ zip (toWalls pps) (repeat Wall)
  where
    toWalls :: [[Pos]] -> [Pos]
    toWalls = concatMap (\ps -> concatMap toLine (zip ps (tail ps)))
    toLine :: (Pos, Pos) -> [Pos]
    toLine ((a, b), (x, y))
      | a == x && b <= y = [(a, j) | j <- [b .. y]]
      | a == x && b > y = [(a, j) | j <- [y .. b]]
      | a <= x && b == y = [(i, b) | i <- [a .. x]]
      | otherwise = [(i, b) | i <- [x .. a]]

linesP :: Parser [[Pos]]
linesP = sepEndBy1 (sepBy1 posP (strP "->")) newline
  where
    posP :: Parser Pos
    posP = do
      x <- intP <* char ','
      y <- intP
      pure (y, x)
