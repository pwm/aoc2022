module AoC.Puzzles.Y2022D12 where

import AoC.Lib.Graph
import AoC.Lib.Grid
import AoC.Lib.Prelude
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA = snd . last . walkUp

solveB :: Grid -> Int
solveB = snd . last . walkDown

type Grid = GridOf (Cell, Int)

data Cell = Start | Cell | End
  deriving stock (Show, Eq)

walkUp :: Grid -> [(Pos, Int)]
walkUp g =
  let nexts :: Pos -> [(Pos, Int)]
      nexts p = [(next, 1) | next <- neighbours4 g p, maxOneUp g p next]
   in dijkstra nexts (== end g) (start g)

walkDown :: Grid -> [(Pos, Int)]
walkDown g =
  let nexts :: Pos -> [(Pos, Int)]
      nexts p = [(next, 1) | next <- neighbours4 g p, maxOneDown g p next]
   in dijkstra nexts (\p -> snd (g ! p) == 0) (end g)

maxOneUp, maxOneDown :: Grid -> Pos -> Pos -> Bool
maxOneUp g cur next = snd (g ! next) <= snd (g ! cur) + 1
maxOneDown g cur next = snd (g ! cur) - 1 <= snd (g ! next)

start, end :: Grid -> Pos
start = head . Map.keys . Map.filter ((== Start) . fst)
end = head . Map.keys . Map.filter ((== End) . fst)

parseCell :: Char -> Maybe (Cell, Int)
parseCell = \case
  'S' -> Just (Start, ord 'a' - 97)
  'E' -> Just (End, ord 'z' - 97)
  c -> Just (Cell, ord c - 97)
