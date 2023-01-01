module AoC.Puzzles.Y2022D24 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Grid
parse = fmap addMeAndGoal . parseGrid parseCell

solveA :: Grid -> ()
solveA g =
  let g' = times 200 (stepIce (mkBounds g, mkEmptyGrid g)) g
   in withIO (ppg g') ()

solveB :: a -> ()
solveB _ = ()

{-
- all ice moves 1 step in their Dir4 every turn (forever the same Dir4)
- when hitting wall it wraps around
- multiple ice can occupy the same cell
- i can move Dir4 or wait

What is the fewest number of minutes required to avoid the blizzards and reach the goal?

(35.02 secs, 50,611,783,328 bytes)

(2.18 secs, 2,424,744,024 bytes)
(0.91 secs, 755,486,176 bytes)
-}

runTurns :: Grid -> (Grid, (Int, Pos))
runTurns g =
  let endPos = head . Map.keys . Map.filter (== Goal)
   in runState (turns g) (0, endPos g)

turns :: Grid -> State (Int, Pos) Grid
turns g = do
  (i, ep) <- get
  if g ! ep == Me
    then pure g
    else do
      let g' = turn1 g
      put (i + 1, ep)
      turns g'

turn1 :: Grid -> Grid
turn1 = undefined

{-
After all ice moved
  if our current pos is still clear
    try moving towards End (manhattan)
    if can't then stay put
  if our current pos got iced
    we have to move somewhere
      try moving closer to End if possible
      otherwise find the "best" next spot (do we need LogicT?)

-}

moveMe :: Pos -> Grid -> [Pos]
moveMe pos g =
  let nexts :: Pos -> [Pos]
      nexts p = filter (\x -> Map.member x g && g ! x `elem` [Air, Goal]) $ adj4 p
   in bfs nexts (const False) pos

-- λ ppg $ times 18 stepIce $ g1
stepIce :: (Bounds, Grid) -> Grid -> Grid
stepIce st g = evalState go st
  where
    go :: State (Bounds, Grid) Grid
    go = do
      (bounds, newGrid) <- get
      let icePoses = getIcePoses g
      pure $ foldr insertIce newGrid (allNewIcePos bounds icePoses)
      where
        insertIce :: (Pos, Dir4) -> Grid -> Grid
        insertIce (p, d) = Map.insertWith addIce p (Ice [d])
        addIce :: Cell -> Cell -> Cell
        addIce new old = case (new, old) of
          (Ice a, Ice b) -> Ice (a <> b)
          (a, _) -> a

getIcePoses :: Grid -> [(Pos, Cell)]
getIcePoses = Map.toList . Map.filter (`notElem` [Wall, Me, Air, Goal])

allNewIcePos :: Bounds -> [(Pos, Cell)] -> [(Pos, Dir4)]
allNewIcePos bounds = concatMap newIcePos
  where
    newIcePos :: (Pos, Cell) -> [(Pos, Dir4)]
    newIcePos (p, c) = case c of
      Ice dirs -> map (\dir -> (step4wrap p dir, dir)) dirs
      _ -> []
    step4wrap :: Pos -> Dir4 -> Pos
    step4wrap p d =
      let p'@(x, y) = step4 p d
       in if
              | p' `elem` bounds.lCol -> (x, bounds.maxY - 1)
              | p' `elem` bounds.rCol -> (x, bounds.minY + 1)
              | p' `elem` bounds.uRow -> (bounds.maxX - 1, y)
              | p' `elem` bounds.dRow -> (bounds.minX + 1, y)
              | otherwise -> p'

data Bounds = Bounds
  { minX :: Int,
    minY :: Int,
    maxX :: Int,
    maxY :: Int,
    lCol :: [Pos],
    rCol :: [Pos],
    uRow :: [Pos],
    dRow :: [Pos]
  }
  deriving stock (Show, Eq)

mkBounds :: Grid -> Bounds
mkBounds g =
  Bounds
    { minX = minX g,
      minY = minY g,
      maxX = maxX g,
      maxY = maxY g,
      lCol = lCol g,
      rCol = rCol g,
      uRow = uRow g,
      dRow = dRow g
    }

lCol, rCol, uRow, dRow :: Grid -> [Pos]
lCol g = [(x, minY g) | x <- [minX g .. maxX g]]
rCol g = [(x, maxY g) | x <- [minX g .. maxX g]]
uRow g = [(minX g, y) | y <- [minY g .. maxY g]]
dRow g = [(maxX g, y) | y <- [minY g .. maxY g]]

minX, minY, maxX, maxY :: Grid -> Int
minX = fst . fst . Map.findMin
minY = minX . Map.fromList . map (first swap) . Map.toList
maxX = fst . fst . Map.findMax
maxY = maxX . Map.fromList . map (first swap) . Map.toList

mkEmptyGrid :: Grid -> Grid
mkEmptyGrid = Map.map $ \case
  Wall -> Wall
  _ -> Air

type Grid = GridOf Cell

data Cell
  = Wall
  | Air
  | Ice [Dir4]
  | Me
  | Goal
  deriving stock (Show, Eq)

addMeAndGoal :: Grid -> Grid
addMeAndGoal g =
  let me = fst $ Map.findMin $ Map.filter (== Air) g
      end = fst $ Map.findMax $ Map.filter (== Air) g
   in Map.insert end Goal (Map.insert me Me g)

parseCell :: Char -> Maybe Cell
parseCell = \case
  '#' -> Just Wall
  '.' -> Just Air
  '^' -> Just $ Ice [U]
  '>' -> Just $ Ice [R]
  'v' -> Just $ Ice [D]
  '<' -> Just $ Ice [L]
  _ -> Nothing

ppg :: Grid -> IO ()
ppg = putStrLn . printGrid ppCell

ppCell :: Cell -> String
ppCell = \case
  Wall -> "#"
  Air -> "."
  Ice [d] -> case d of U -> "^"; R -> ">"; D -> "v"; L -> "<"
  Ice ds -> show (length ds)
  Me -> "E"
  Goal -> "."

-- for test data

g0, g1 :: Grid
g0 = Map.fromList [((0, 0), Wall), ((0, 1), Me), ((0, 2), Wall), ((0, 3), Wall), ((0, 4), Wall), ((0, 5), Wall), ((0, 6), Wall), ((1, 0), Wall), ((1, 1), Air), ((1, 2), Air), ((1, 3), Air), ((1, 4), Air), ((1, 5), Air), ((1, 6), Wall), ((2, 0), Wall), ((2, 1), Ice [R]), ((2, 2), Air), ((2, 3), Air), ((2, 4), Air), ((2, 5), Air), ((2, 6), Wall), ((3, 0), Wall), ((3, 1), Air), ((3, 2), Air), ((3, 3), Air), ((3, 4), Air), ((3, 5), Air), ((3, 6), Wall), ((4, 0), Wall), ((4, 1), Air), ((4, 2), Air), ((4, 3), Air), ((4, 4), Ice [D]), ((4, 5), Air), ((4, 6), Wall), ((5, 0), Wall), ((5, 1), Air), ((5, 2), Air), ((5, 3), Air), ((5, 4), Air), ((5, 5), Air), ((5, 6), Wall), ((6, 0), Wall), ((6, 1), Wall), ((6, 2), Wall), ((6, 3), Wall), ((6, 4), Wall), ((6, 5), Goal), ((6, 6), Wall)]
g1 = Map.fromList [((0, 0), Wall), ((0, 1), Me), ((0, 2), Wall), ((0, 3), Wall), ((0, 4), Wall), ((0, 5), Wall), ((0, 6), Wall), ((0, 7), Wall), ((1, 0), Wall), ((1, 1), Ice [R]), ((1, 2), Ice [R]), ((1, 3), Air), ((1, 4), Ice [L]), ((1, 5), Ice [U]), ((1, 6), Ice [L]), ((1, 7), Wall), ((2, 0), Wall), ((2, 1), Air), ((2, 2), Ice [L]), ((2, 3), Air), ((2, 4), Air), ((2, 5), Ice [L]), ((2, 6), Ice [L]), ((2, 7), Wall), ((3, 0), Wall), ((3, 1), Ice [R]), ((3, 2), Ice [D]), ((3, 3), Air), ((3, 4), Ice [R]), ((3, 5), Ice [L]), ((3, 6), Ice [R]), ((3, 7), Wall), ((4, 0), Wall), ((4, 1), Ice [L]), ((4, 2), Ice [U]), ((4, 3), Ice [D]), ((4, 4), Ice [U]), ((4, 5), Ice [U]), ((4, 6), Ice [R]), ((4, 7), Wall), ((5, 0), Wall), ((5, 1), Wall), ((5, 2), Wall), ((5, 3), Wall), ((5, 4), Wall), ((5, 5), Wall), ((5, 6), Goal), ((5, 7), Wall)]

s0 :: String
s0 =
  unpack
    [trimming|
#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#
|]

s1 :: String
s1 =
  unpack
    [trimming|
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
|]
