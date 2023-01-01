module AoC.Puzzles.Y2022D22 where

import AoC.Lib.Display
import AoC.Lib.Grid (Dir4 (..), GridOf, Pos, listToGrid, (<+>))
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe (Grid, [Step])
parse = parseMaybe (optional newline *> inputP)

-- 122_082
solveA :: (Grid, [Step]) -> Int
solveA = calcPassword . uncurry travel

solveB :: a -> ()
solveB _ = ()

calcPassword :: Grid -> Int
calcPassword grid =
  let dirVal :: Dir4 -> Int
      dirVal = \case R -> 0; D -> 1; L -> 2; U -> 3
      ((row, col), myDir) = getMe grid
   in 1000 * (row + 1) + 4 * (col + 1) + dirVal myDir

travel :: Grid -> [Step] -> Grid
travel = foldl' (flip move)

move :: Step -> Grid -> Grid
move s grid =
  let (myPos, myDir) = getMe grid
   in case s of
        Step n -> step n myPos myDir grid
        Turn t -> Map.insert myPos (Me (turn (myDir, t))) grid

step :: Int -> Pos -> Dir4 -> Grid -> Grid
step 0 _ _ grid = grid
step n from dir grid =
  let to = wrapStep grid dir (stepTo from dir)
   in case grid !? to of
        Just cell | cell == Wall -> grid -- stop as we've bumped into a wall
        Just _ -> step (n - 1) to dir $ Map.insert to (Me dir) (Map.insert from Air grid)
        Nothing -> error "Pos must exist (it must wrap around)"

wrapStep :: Grid -> Dir4 -> Pos -> Pos
wrapStep grid dir to@(row, col) = case grid !? to of
  Just _ -> to
  Nothing -> case dir of
    L -> (row, maximum (colsInRow row grid))
    U -> (maximum (rowsInCol col grid), col)
    R -> (row, minimum (colsInRow row grid))
    D -> (minimum (rowsInCol col grid), col)

colsInRow, rowsInCol :: Int -> Grid -> [Int]
colsInRow r = map snd . filter ((== r) . fst) . Map.keys
rowsInCol c = map fst . filter ((== c) . snd) . Map.keys

getMe :: Grid -> (Pos, Dir4)
getMe = second (fromJust . getMyDir) . head . Map.toList . Map.filter findMe
  where
    findMe :: Cell -> Bool
    findMe = \case Me _ -> True; _ -> False
    getMyDir :: Cell -> Maybe Dir4
    getMyDir = \case Me dir -> Just dir; _ -> Nothing

stepTo :: Pos -> Dir4 -> Pos
stepTo pos = \case
  L -> pos <+> (0, -1)
  U -> pos <+> (-1, 0)
  R -> pos <+> (0, 1)
  D -> pos <+> (1, 0)

turn :: (Dir4, Turn) -> Dir4
turn = \case
  (L, TL) -> D
  (D, TL) -> R
  (R, TL) -> U
  (U, TL) -> L
  (L, TR) -> U
  (U, TR) -> R
  (R, TR) -> D
  (D, TR) -> L

type Grid = GridOf Cell

data Step = Step Int | Turn Turn
  deriving stock (Show, Eq, Ord)

data Turn = TL | TR
  deriving stock (Show, Eq, Ord, Bounded, Enum)

data Cell = Wall | Air | Me Dir4
  deriving stock (Show, Eq, Ord)

inputP :: Parser (Grid, [Step])
inputP = do
  (gridLines, head -> steps) <-
    partitionEithers
      <$> sepEndBy
        (Left <$> gridLineP <|> Right <$> some stepP)
        (some newline)
  let parseCell :: Char -> Cell
      parseCell = \case
        '#' -> Wall
        '.' -> Air
        _ -> error "Must be either air or wall at this point"
      maxLineLength = maximum (map length gridLines)
      grid =
        Map.updateMin (const (Just (Me R)))
          . Map.map parseCell
          . Map.filter (/= ' ')
          . listToGrid
          . map (pad maxLineLength)
          $ gridLines
  pure (grid, steps)

gridLineP :: Parser String
gridLineP = some (choice (map char [' ', '.', '#']))

stepP :: Parser Step
stepP = Step <$> intP0 <|> Turn <$> turnP

turnP :: Parser Turn
turnP =
  enumParser
    (\case TL -> "L"; TR -> "R")
    (\case "L" -> Just TL; "R" -> Just TR; _ -> Nothing)

ppg :: Grid -> IO ()
ppg = displayGrid ppCell

ppCell :: Cell -> String
ppCell = \case
  Wall -> "#"
  Air -> "."
  Me dir -> case dir of U -> "^"; R -> ">"; D -> "v"; L -> "<"

-- 6032
-- ((5,7), Me R)
s0 :: String
s0 =
  unpack
    [untrimming|
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
|]

m0 :: (Grid, [Step])
m0 =
  ( Map.fromList [((0, 8), Me R), ((0, 9), Air), ((0, 10), Air), ((0, 11), Wall), ((1, 8), Air), ((1, 9), Wall), ((1, 10), Air), ((1, 11), Air), ((2, 8), Wall), ((2, 9), Air), ((2, 10), Air), ((2, 11), Air), ((3, 8), Air), ((3, 9), Air), ((3, 10), Air), ((3, 11), Air), ((4, 0), Air), ((4, 1), Air), ((4, 2), Air), ((4, 3), Wall), ((4, 4), Air), ((4, 5), Air), ((4, 6), Air), ((4, 7), Air), ((4, 8), Air), ((4, 9), Air), ((4, 10), Air), ((4, 11), Wall), ((5, 0), Air), ((5, 1), Air), ((5, 2), Air), ((5, 3), Air), ((5, 4), Air), ((5, 5), Air), ((5, 6), Air), ((5, 7), Air), ((5, 8), Wall), ((5, 9), Air), ((5, 10), Air), ((5, 11), Air), ((6, 0), Air), ((6, 1), Air), ((6, 2), Wall), ((6, 3), Air), ((6, 4), Air), ((6, 5), Air), ((6, 6), Air), ((6, 7), Wall), ((6, 8), Air), ((6, 9), Air), ((6, 10), Air), ((6, 11), Air), ((7, 0), Air), ((7, 1), Air), ((7, 2), Air), ((7, 3), Air), ((7, 4), Air), ((7, 5), Air), ((7, 6), Air), ((7, 7), Air), ((7, 8), Air), ((7, 9), Air), ((7, 10), Wall), ((7, 11), Air), ((8, 8), Air), ((8, 9), Air), ((8, 10), Air), ((8, 11), Wall), ((8, 12), Air), ((8, 13), Air), ((8, 14), Air), ((8, 15), Air), ((9, 8), Air), ((9, 9), Air), ((9, 10), Air), ((9, 11), Air), ((9, 12), Air), ((9, 13), Wall), ((9, 14), Air), ((9, 15), Air), ((10, 8), Air), ((10, 9), Wall), ((10, 10), Air), ((10, 11), Air), ((10, 12), Air), ((10, 13), Air), ((10, 14), Air), ((10, 15), Air), ((11, 8), Air), ((11, 9), Air), ((11, 10), Air), ((11, 11), Air), ((11, 12), Air), ((11, 13), Air), ((11, 14), Wall), ((11, 15), Air)],
    [Step 10, Turn TR, Step 5, Turn TL, Step 5, Turn TR, Step 10, Turn TL, Step 4, Turn TR, Step 5, Turn TL, Step 5]
  )

m1 :: (Grid, [Step])
{-# NOINLINE m1 #-}
m1 = unsafePerformIO (fromJust . parse <$> load)
