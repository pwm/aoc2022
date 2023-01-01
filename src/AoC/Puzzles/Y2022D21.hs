module AoC.Puzzles.Y2022D21 where

import AoC.Lib.Parser
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe (Map String Line)
parse =
  fmap Map.fromList
    . parseMaybe (sepEndBy1 lineP newline)

solveA :: Map String Line -> Int
solveA = fromIntegral . eval . buildExpr

-- subtract 1 cause of Haskell's "fair" rounding (round @Double @Integer 0.5 == 0)
solveB :: Map String Line -> Int
solveB = subtract 1 . binarySearch

data Expr
  = Num Integer
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  deriving stock (Show)

binarySearch :: Map String Line -> Int
binarySearch m = fromMaybe 0 $ go 0 maxBound
  where
    go :: Int -> Int -> Maybe Int
    go low high
      | high < low = Nothing
      | lhs < rhs = go low (n - 1)
      | lhs > rhs = go (n + 1) high
      | otherwise = Just n
      where
        n = low + (high - low) `div` 2
        (lhs, rhs) = case yellNum n m of
          a :+: b -> (eval a, eval b)
          _ -> error "Root must be addition"

yellNum :: Int -> Map String Line -> Expr
yellNum n = buildExpr . Map.insert "humn" (LNum (fromIntegral n))

eval :: Expr -> Integer
eval = \case
  Num n -> n
  lhs :+: rhs -> eval lhs + eval rhs
  lhs :-: rhs -> eval lhs - eval rhs
  lhs :*: rhs -> eval lhs * eval rhs
  lhs :/: rhs -> eval lhs `div` eval rhs

buildExpr :: Map String Line -> Expr
buildExpr m = go (m ! "root")
  where
    go :: Line -> Expr
    go = \case
      LNum n -> Num n
      LAdd lhs rhs -> go (m ! lhs) :+: go (m ! rhs)
      LSub lhs rhs -> go (m ! lhs) :-: go (m ! rhs)
      LMul lhs rhs -> go (m ! lhs) :*: go (m ! rhs)
      LDiv lhs rhs -> go (m ! lhs) :/: go (m ! rhs)

data Line
  = LNum Integer
  | LAdd String String
  | LSub String String
  | LMul String String
  | LDiv String String
  deriving stock (Show)

lineP :: Parser (String, Line)
lineP = do
  ident <- count 4 lowerChar <* strP ":"
  val <- LNum <$> (fromIntegral <$> intP) <|> lineValP
  pure (ident, val)

lineValP :: Parser Line
lineValP = do
  lhs <- count 4 lowerChar
  op <- char ' ' *> choice (map char ['+', '-', '*', '/']) <* char ' '
  rhs <- count 4 lowerChar
  let parseOp :: Char -> Maybe Line
      parseOp = \case
        '+' -> Just (LAdd lhs rhs)
        '-' -> Just (LSub lhs rhs)
        '*' -> Just (LMul lhs rhs)
        '/' -> Just (LDiv lhs rhs)
        _ -> Nothing
  maybeToP parseOp op
