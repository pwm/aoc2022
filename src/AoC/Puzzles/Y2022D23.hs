module AoC.Puzzles.Y2022D23 where

import AoC.Lib.Grid hiding (move, neighbours8)
import AoC.Lib.Prelude
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe (Set Pos)
parse = fmap (Map.keysSet . Map.filter (/= '.')) . parseGrid Just

solveA :: Set Pos -> Int
solveA = emptyCells . flip evalState (0, [N, S, W, E]) . timesM 10 turn1

solveB :: Set Pos -> Int
solveB = fst . flip execState (0, [N, S, W, E]) . fixpointM turn1

emptyCells :: Set Pos -> Int
emptyCells s = (maxX s - minX s + 1) * (maxY s - minY s + 1) - Set.size s

turn1 :: Set Pos -> State (Int, [Dir8]) (Set Pos)
turn1 s = do
  (i, dirs) <- get
  let s' = move s . filterCollisions . proposedMoves dirs . canMove $ s
  put (i + 1, rotate dirs)
  pure s'

move :: Set Pos -> [(Pos, Pos)] -> Set Pos
move = foldr (\(from, to) -> Set.insert to . Set.delete from)

filterCollisions :: [(Pos, Pos)] -> [(Pos, Pos)]
filterCollisions =
  map swap
    . Map.toList
    . Map.map head
    . Map.filter (\v -> length v == 1)
    . foldr ((\(to, from) -> Map.insertWith (<>) to [from]) . swap) mempty

proposedMoves :: [Dir8] -> Set Pos -> [(Pos, Pos)]
proposedMoves dirs s = map (\p -> (p, newPos p)) (Set.toList s)
  where
    newPos :: Pos -> Pos
    newPos p = headOr p $ mapMaybe posCheck dirs
      where
        posCheck :: Dir8 -> Maybe Pos
        posCheck = \case
          N | checkDirs [stepNE, stepN, stepNW] -> Just $ p <+> stepN
          S | checkDirs [stepSE, stepS, stepSW] -> Just $ p <+> stepS
          W | checkDirs [stepNW, stepW, stepSW] -> Just $ p <+> stepW
          E | checkDirs [stepNE, stepE, stepSE] -> Just $ p <+> stepE
          _ -> Nothing
        checkDirs :: [Pos] -> Bool
        checkDirs ds = null $ setLookups s ((p <+>) <$> ds)

canMove :: Set Pos -> Set Pos
canMove s = Set.filter (not . null . neighbours8 s) s

neighbours8 :: Set Pos -> Pos -> [Pos]
neighbours8 s = filter (`Set.member` s) . adj8

minX, minY, maxX, maxY :: Set Pos -> Int
minX = fst . Set.findMin
maxX = fst . Set.findMax
minY = minX . Set.fromList . map swap . Set.toList
maxY = maxX . Set.fromList . map swap . Set.toList
