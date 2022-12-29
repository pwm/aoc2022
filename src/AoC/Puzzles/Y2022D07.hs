module AoC.Puzzles.Y2022D07 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (many, some)
import Data.Tree

parse :: String -> Maybe (Tree Node)
parse = parseMaybe treeP

solveA :: Tree Node -> Int
solveA = sum . filter (< 100000) . map getDirSize . flatten . addDirSize

solveB :: Tree Node -> Int
solveB t =
  let dirSizes = rsort . map getDirSize . flatten . addDirSize $ t
   in last $ filter (> 30_000_000 + head dirSizes - 70_000_000) dirSizes

data Node
  = File Int String
  | Dir Int String
  deriving stock (Show)

getDirSize :: Node -> Int
getDirSize (Dir size _) = size
getDirSize (File _ _) = 0

addDirSize :: Tree Node -> Tree Node
addDirSize = foldTree f
  where
    f :: Node -> [Tree Node] -> Tree Node
    f (Dir _ name) ts = Node (Dir (sum (map (foldTree g) ts)) name) ts
    f file ts = Node file ts
    g :: Node -> [Int] -> Int
    g (Dir size _) _ = size
    g (File size _) sizes = size + sum sizes

treeP :: Parser (Tree Node)
treeP = do
  dir <- strP "$ cd" *> (strP "/" <|> some lowerChar) <* newline
  void $ strP "$ ls" <* newline
  files <- catMaybes <$> sepEndBy (Just <$> fileP <|> Nothing <$ dirP) newline
  dirs <- many (try treeP)
  void (strP "$ cd .." <* newline) <|> eof
  pure $ Node (Dir 0 dir) (map (`Node` []) files <> dirs)
  where
    fileP, dirP :: Parser Node
    fileP = liftA2 File intP (some (char '.' <|> lowerChar))
    dirP = fmap (Dir 0) (strP "dir" *> some lowerChar)

ppTree :: Tree Node -> String
ppTree = drawTree . foldTree (Node . ppNode)
  where
    ppNode :: Node -> String
    ppNode = \case
      File size name -> name <> " (" <> show size <> ")"
      Dir size name -> name <> " (" <> show size <> ")"
