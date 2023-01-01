module AoC.Puzzles.Y2022D17 where

import AoC.Lib.Grid (GridOf, Pos, printGrid, (<+>))
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe [Dir3]
parse = traverse parseDir . dropEnd 1

solveA :: [Dir3] -> Int
solveA dirs = view _2 $ fst $ cycleN dirs 2022 (1, 0, 0) board0

solveB :: [Dir3] -> ()
solveB _ = ()

type Board = GridOf Cell

type Shape = [(Pos, Cell)]

data Cell = A | S | W
  deriving stock (Show, Eq)

data Dir3 = L | R | D
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

{- ORMOLU_DISABLE -}
{-
8|.......|  8|.......|  8|.......|  8|.......|  8|.......|  8|.......|
7|.......|  7|.......|  7|.......|  7|.......|  7|..@....|  7|.......|
6|.......|  6|.......|  6|...@...|  6|....@..|  6|..@....|  6|.......|
5|.......|  5|.......|  5|..@@@..|  5|....@..|  5|..@....|  5|..@@...|
4|.......|  4|..@@@@.|  4|...@...|  4|..@@@..|  4|..@....|  4|..@@...|
3|.......|  3|.......|  3|.......|  3|.......|  3|.......|  3|.......|
2|.......|  2|.......|  2|.......|  2|.......|  2|.......|  2|.......|
1|.......|  1|.......|  1|.......|  1|.......|  1|.......|  1|.......|
0+-------+  0+-------+  0+-------+  0+-------+  0+-------+  0+-------+
 012345678   012345678   012345678   012345678   012345678   012345678
-}
board0 :: Board
board0 = Map.fromList [
  ((0,8),W),((1,8),A),((2,8),A),((3,8),A),((4,8),A),((5,8),A),((6,8),A),((7,8),A),((8,8),W),
  ((0,7),W),((1,7),A),((2,7),A),((3,7),A),((4,7),A),((5,7),A),((6,7),A),((7,7),A),((8,7),W),
  ((0,6),W),((1,6),A),((2,6),A),((3,6),A),((4,6),A),((5,6),A),((6,6),A),((7,6),A),((8,6),W),
  ((0,5),W),((1,5),A),((2,5),A),((3,5),A),((4,5),A),((5,5),A),((6,5),A),((7,5),A),((8,5),W),
  ((0,4),W),((1,4),A),((2,4),A),((3,4),A),((4,4),A),((5,4),A),((6,4),A),((7,4),A),((8,4),W),
  ((0,3),W),((1,3),A),((2,3),A),((3,3),A),((4,3),A),((5,3),A),((6,3),A),((7,3),A),((8,3),W),
  ((0,2),W),((1,2),A),((2,2),A),((3,2),A),((4,2),A),((5,2),A),((6,2),A),((7,2),A),((8,2),W),
  ((0,1),W),((1,1),A),((2,1),A),((3,1),A),((4,1),A),((5,1),A),((6,1),A),((7,1),A),((8,1),W),
  ((0,0),W),((1,0),W),((2,0),W),((3,0),W),((4,0),W),((5,0),W),((6,0),W),((7,0),W),((8,0),W)
  ]
{- ORMOLU_ENABLE -}

cycleN :: [Dir3] -> Int -> (Int, Int, Int) -> Board -> ((Int, Int, Int), Board)
cycleN dirs =
  let infDirs = cycle dirs
      go :: Int -> Int -> (Int, Int, Int) -> Board -> ((Int, Int, Int), Board)
      go iter maxIter info0 b0
        | iter == maxIter = (info0, b0)
        | otherwise =
            let (info1, b1) = cycle1 infDirs info0 b0
             in go (iter + 1) maxIter info1 b1
   in go 0

cycle1 :: [Dir3] -> (Int, Int, Int) -> Board -> ((Int, Int, Int), Board)
cycle1 dirs (shapeId, curOrigin, dirIdx) b0 =
  let shape = shapeAt (shapes ! shapeId) curOrigin
      go :: Int -> [Dir3] -> Board -> (Board, Int)
      go idx [] b = (b, idx)
      go idx (dir : ds) b
        | null (getShape b) = (b, idx)
        | otherwise = go (idx + 1) ds (move D (move dir b))
      (b', dirIdx') =
        go
          dirIdx
          (drop dirIdx dirs)
          (putShape shape (upBoard shape b0))
      shapeId' = if shapeId == 5 then 1 else shapeId + 1
   in ((shapeId', maxFloor b', dirIdx'), b')

move :: Dir3 -> Board -> Board
move dir b
  | cellsNotOf W b to = moveShape from to b
  | otherwise = case dir of
      D -> turnShapeToWall b
      _ -> b
  where
    from = getShape b
    to = addToShapePos pDelta from
    pDelta = case dir of
      L -> (-1, 0)
      R -> (1, 0)
      D -> (0, -1)

cellsNotOf :: Cell -> Board -> Shape -> Bool
cellsNotOf cell b = notElem cell . lookups b . map fst

turnShapeToWall :: Board -> Board
turnShapeToWall b = putShape (shapeCellsTo W (getShape b)) b

moveShape :: Shape -> Shape -> Board -> Board
moveShape from to = putShape (shapeCellsTo S to) . putShape (shapeCellsTo A from)

addToShapePos :: Pos -> Shape -> Shape
addToShapePos pos = map (first (<+> pos))

shapeCellsTo :: Cell -> Shape -> Shape
shapeCellsTo cell = map (second (const cell))

getShape :: Board -> Shape
getShape = Map.toList . Map.filter (== S)

putShape :: Shape -> Board -> Board
putShape s b = foldr (uncurry Map.insert) b s

shapeAt :: (Pos -> Shape) -> Int -> Shape
shapeAt shape = shape . origin

origin :: Int -> Pos
origin y = (3, y + 4)

upBoard :: Shape -> Board -> Board
upBoard s b =
  let r y = [((0, y), W)] <> [((x, y), A) | x <- [1 :: Int .. 7]] <> [((8, y), W)]
      shapeHeight = let ys = map (snd . fst) s in maximum ys - minimum ys + 1
      ys' = [maxFloor b + 1 .. maxFloor b + 1 + shapeHeight + 3]
   in putShape (concatMap r ys') b

maxFloor :: Board -> Int
maxFloor =
  maximum
    . map snd
    . filter (\(x, _) -> x `elem` [1 .. 7])
    . Map.keys
    . Map.filter (== W)

shapes :: Map Int (Pos -> Shape)
shapes =
  Map.fromList
    [ (1, shape1),
      (2, shape2),
      (3, shape3),
      (4, shape4),
      (5, shape5)
    ]

shape1 :: Pos -> Shape
shape1 (x, y) = [((a, y), S) | a <- [x .. x + 3]]

shape2 :: Pos -> Shape
shape2 (x, y) =
  let r1 = [((x, y), A), ((x + 1, y), S), ((x + 2, y), A)]
      r2 = [((x, y + 1), S), ((x + 1, y + 1), S), ((x + 2, y + 1), S)]
      r3 = [((x, y + 2), A), ((x + 1, y + 2), S), ((x + 2, y + 2), A)]
   in mconcat [r1, r2, r3]

shape3 :: Pos -> Shape
shape3 (x, y) =
  let r1 = [((x, y), S), ((x + 1, y), S), ((x + 2, y), S)]
      r2 = [((x, y + 1), A), ((x + 1, y + 1), A), ((x + 2, y + 1), S)]
      r3 = [((x, y + 2), A), ((x + 1, y + 2), A), ((x + 2, y + 2), S)]
   in mconcat [r1, r2, r3]

shape4 :: Pos -> Shape
shape4 (x, y) = [((x, b), S) | b <- [y .. y + 3]]

shape5 :: Pos -> Shape
shape5 (x, y) = [((a, b), S) | a <- [x .. x + 1], b <- [y .. y + 1]]

parseDir :: Char -> Maybe Dir3
parseDir = \case
  '<' -> Just L
  '>' -> Just R
  _ -> Nothing

ppb :: Board -> IO ()
ppb = putStrLn . ppBoard

ppBoard :: Board -> String
ppBoard = unlines . transpose . map reverse . lines . printGrid ppCell

ppCell :: Cell -> String
ppCell = \case
  A -> "."
  S -> "@"
  W -> "+"

--

s0 :: String
s0 =
  unpack
    [trimming|
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
|]

d0 :: [Dir3]
d0 = [R, R, R, L, L, R, L, R, R, L, L, L, R, R, L, R, R, R, L, L, L, R, R, R, L, L, L, R, L, L, L, R, R, L, R, R, L, L, R, R]
