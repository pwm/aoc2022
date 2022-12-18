module AoC.Puzzles.Y2022D18 where

import AoC.Lib.Graph
import AoC.Lib.Prelude
import Data.Set qualified as Set

parse :: String -> Maybe (Set P3)
parse = fmap Set.fromList . traverse l2p3 <=< traverse (stringToIntsSepBy ",") . lines

solveA :: Set P3 -> Int
solveA = surface

solveB :: Set P3 -> Int
solveB pSet = surface pSet - surface (internals pSet)

type P3 = (Int, Int, Int)

surface :: Set P3 -> Int
surface pSet = sum (map (length . getAirNs pSet) (Set.toList pSet))

externals :: Set P3 -> Set P3
externals pSet =
  let bb = mkBB (bbSize pSet)
      nexts :: P3 -> [P3]
      nexts = filter (`Set.member` bb) . getAirNs pSet
   in Set.fromList $ bfs nexts (const False) (0, 0, 0)

internals :: Set P3 -> Set P3
internals pSet =
  mkBB (bbSize pSet) `Set.difference` (pSet `Set.union` externals pSet)

mkBB :: Int -> Set P3
mkBB size =
  let l :: [Int] = [0 .. size]
   in Set.fromList [(x, y, z) | x <- l, y <- l, z <- l]

bbSize :: Set P3 -> Int
bbSize = maximum . concatMap t2l . Set.toList

getAirNs :: Set P3 -> P3 -> [P3]
getAirNs pSet = filter (`Set.notMember` pSet) . n6

n6 :: P3 -> [P3]
n6 (a, b, c) =
  let rels = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
   in [(a + x, b + y, c + z) | (x, y, z) <- rels]
