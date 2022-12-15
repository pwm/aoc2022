module AoC.Puzzles.Y2022D15 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [((Pos, Pos), Int)]
parse = parseMaybe (sepEndBy1 lineP newline)

solveA :: [((Pos, Pos), Int)] -> Int
solveA = noBeaconCountAtRow 2_000_000

solveB :: [((Pos, Pos), Int)] -> ()
solveB _ = ()

noBeaconCountAtRow :: Int -> [((Pos, Pos), Int)] -> Int
noBeaconCountAtRow pX sbms0 =
  let sYs = map (snd . fst . fst) sbms0
      ms = map snd sbms0
      pYs = [minimum (zipWith (-) sYs ms) .. maximum (zipWith (+) sYs ms)]
   in subtract 1 $ foldl' (go sbms0) 0 (zip (repeat pX) pYs)
  where
    go :: [((Pos, Pos), Int)] -> Int -> Pos -> Int
    go [] acc _ = acc
    go (sbm : sbms) acc p
      | ((s, _), md) <- sbm, manhattan p s <= md = acc + 1
      | otherwise = go sbms acc p

lineP :: Parser ((Pos, Pos), Int)
lineP = do
  sY <- strP "Sensor at x=" *> signedIntP
  sX <- strP ", y=" *> signedIntP
  bY <- strP ": closest beacon is at x=" *> signedIntP
  bX <- strP ", y=" *> signedIntP
  pure (((sX, sY), (bX, bY)), manhattan (sX, sY) (bX, bY))
