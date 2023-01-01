module AoC.Puzzles.Y2022D15 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [((Pos, Pos), Int)]
parse = parseMaybe (sepEndBy1 lineP newline)

solveA :: [((Pos, Pos), Int)] -> Int
solveA = noBeaconCountAtRow 2_000_000

solveB :: [((Pos, Pos), Int)] -> Int
solveB sensors =
  let (x, y) = distressBeacon sensors
   in y * 4000000 + x

distressBeacon :: [((Pos, Pos), Int)] -> Pos
distressBeacon sensors =
  headOr (0, 0) . nubOrd $
    filter (not . covered sensors) (concatMap outerEdges sensors)

noBeaconCountAtRow :: Int -> [((Pos, Pos), Int)] -> Int
noBeaconCountAtRow pX sensors =
  let sYs = map (snd . fst . fst) sensors
      ms = map snd sensors
      pYs = [minimum (zipWith (-) sYs ms) .. maximum (zipWith (+) sYs ms)]
      coverCount :: Int -> Pos -> Int
      coverCount c p = if covered sensors p then c + 1 else c
   in subtract 1 $ foldl' coverCount 0 (zip (repeat pX) pYs)

covered :: [((Pos, Pos), Int)] -> Pos -> Bool
covered [] _ = False
covered (((sensorPos, _), distSB) : sensors) pos
  | manhattan sensorPos pos <= distSB = True
  | otherwise = covered sensors pos

outerEdges :: ((Pos, Pos), Int) -> [Pos]
outerEdges (((x, y), _), m) =
  concat
    [ zip [minX .. x] [y, y - 1 .. minY],
      zip [x .. maxX] [minY .. y],
      zip [maxX, maxX - 1 .. x] [y .. maxY],
      zip [x, x - 1 .. minX] [maxY, maxY - 1 .. y]
    ]
  where
    minX = max (x - m - 1) 0
    maxX = min (x + m + 1) 4_000_000
    minY = max (y - m - 1) 0
    maxY = min (y + m + 1) 4_000_000

lineP :: Parser ((Pos, Pos), Int)
lineP = do
  sY <- strP "Sensor at x=" *> signedIntP
  sX <- strP ", y=" *> signedIntP
  bY <- strP ": closest beacon is at x=" *> signedIntP
  bX <- strP ", y=" *> signedIntP
  pure (((sX, sY), (bX, bY)), manhattan (sX, sY) (bX, bY))
