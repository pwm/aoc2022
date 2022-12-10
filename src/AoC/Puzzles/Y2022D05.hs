module AoC.Puzzles.Y2022D05 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (some)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map

parse :: String -> Maybe (Crates, [Move])
parse = parseMaybe inputP

solveA :: (Crates, [Move]) -> String
solveA = topCrates . uncurry (run CrateMover9000)

solveB :: (Crates, [Move]) -> String
solveB = topCrates . uncurry (run CrateMover9001)

type Crates = Map Int String

data Move = Move
  { quantity :: Int,
    from :: Int,
    to :: Int
  }
  deriving stock (Show)

data Machine = CrateMover9000 | CrateMover9001
  deriving stock (Show)

run :: Machine -> Crates -> [Move] -> Crates
run machine = foldl' $ \crates move ->
  let (toMove, toKeep) = splitAt move.quantity (crates ! move.from)
      stacker = case machine of
        CrateMover9000 -> flip (foldl' (flip (:)))
        CrateMover9001 -> (<>)
   in Map.insertWith stacker move.to toMove (Map.insert move.from toKeep crates)

topCrates :: Crates -> String
topCrates = concatMap (take 1) . Map.elems

-- Parser

inputP :: Parser (Crates, [Move])
inputP = do
  crateRows <- sepEndBy1 (try (sepBy1 crateP (char ' '))) newline
  stackIds <- sc *> some intP <* some newline
  moves <- sepEndBy1 moveP newline
  let toCrates :: [String] -> Crates
      toCrates = Map.fromList . zip stackIds . map (filter (/= '_')) . transpose
  pure (toCrates crateRows, moves)

crateP :: Parser Char
crateP = try (squareBracketsP0 upperChar) <|> (count 3 (char ' ') $> '_')

moveP :: Parser Move
moveP = do
  quantity <- string "move " *> intP
  from <- string "from " *> intP
  to <- string "to " *> intP
  pure $ Move quantity from to
