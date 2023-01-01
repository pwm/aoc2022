module AoC.Puzzles.Y2022D09 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Set qualified as Set

parse :: String -> Maybe [(Dir4, Int)]
parse = parseMaybe (sepEndBy1 lineP newline)

solveA :: [(Dir4, Int)] -> Int
solveA = Set.size . moveSnake (replicate 2 (0, 0))

solveB :: [(Dir4, Int)] -> Int
solveB = Set.size . moveSnake (replicate 10 (0, 0))

moveSnake :: [Pos] -> [(Dir4, Int)] -> Set Pos
moveSnake snake moves = execState (foldM snakeMove snake moves) mempty

snakeMove :: [Pos] -> (Dir4, Int) -> State (Set Pos) [Pos]
snakeMove snake (dir, i) = do
  let snakes = timesAcc i (snakeStep dir) snake
  modify $ \s -> foldr (Set.insert . last) s snakes
  pure (last snakes)

snakeStep :: Dir4 -> [Pos] -> [Pos]
snakeStep dir snake =
  let pullTail :: [Pos] -> Pos -> [Pos]
      pullTail hs t = hs <> [follow (last hs) t]
   in foldl' pullTail [step4 (head snake) dir] (tail snake)

follow :: Pos -> Pos -> Pos
follow h t
  | dist h t `elem` n9 = t
  | otherwise = t <+> closer h t

closer :: Pos -> Pos -> Pos
closer to p = head (decoSort (manhattan (to <-> p)) n8)

lineP :: Parser (Dir4, Int)
lineP = do
  m <- enumParser show parseDir <* char ' '
  i <- intP0
  pure (m, i)

parseDir :: String -> Maybe Dir4
parseDir = \case
  "U" -> Just U
  "R" -> Just R
  "D" -> Just D
  "L" -> Just L
  _ -> Nothing
