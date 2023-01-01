module AoC.Puzzles.Y2022D11 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id, lcm, round, (!))
import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as IntMap

parse :: String -> Maybe (IntMap Monkey)
parse = fmap toMap . parseMaybe (sepEndBy1 monkeyP newline)
  where
    toMap :: [Monkey] -> IntMap Monkey
    toMap = foldr (\monkey m -> IntMap.insert monkey.id monkey m) mempty

solveA :: IntMap Monkey -> Int
solveA = monkeyBusinessLevel . times 20 (round A)

solveB :: IntMap Monkey -> Int
solveB = monkeyBusinessLevel . times 10000 (round B)

monkeyBusinessLevel :: IntMap Monkey -> Int
monkeyBusinessLevel = product . take 2 . rsort . map (view #cc) . IntMap.elems

data Monkey = Monkey
  { id :: Int,
    items :: [Int],
    op :: Op,
    test :: Test,
    cc :: Int
  }
  deriving stock (Show, Generic)

data Op = Add Arg | Mul Arg
  deriving stock (Show, Generic)

data Arg = Int Int | Old
  deriving stock (Show, Generic)

data Test = Test Int Int Int
  deriving stock (Show, Generic)

data Part = A | B

round :: Part -> IntMap Monkey -> IntMap Monkey
round part ms = execState (traverse (step part) (IntMap.keys ms)) ms

step :: Part -> Int -> State (IntMap Monkey) ()
step part id = do
  let processItem :: Monkey -> Int -> IntMap Monkey -> IntMap Monkey
      processItem m i ms =
        let i' = case part of
              A -> runOp i m.op `div` 3
              B ->
                -- test values are primes so lcm is their product
                let lcm = product ((\(Test c _ _) -> c) . view #test <$> IntMap.elems ms)
                 in runOp i m.op `mod` lcm
            Test cond t f = m.test
         in moveItem id (if i' `mod` cond == 0 then t else f) i' ms
  modify $ \ms -> let m = ms ! id in foldr (processItem m) ms m.items

moveItem :: Int -> Int -> Int -> IntMap Monkey -> IntMap Monkey
moveItem fromId toId item =
  let from, to :: Monkey -> Monkey
      from = over #items (drop 1) . over #cc (+ 1)
      to = over #items (item :)
   in IntMap.adjust from fromId . IntMap.adjust to toId

runOp :: Int -> Op -> Int
runOp x = \case
  Add Old -> x + x
  Mul Old -> x * x
  Add (Int y) -> x + y
  Mul (Int y) -> x * y

monkeyP :: Parser Monkey
monkeyP = do
  id <- strP "Monkey" *> intP <* string ":" <* newline
  items <- sc *> strP "Starting items:" *> sepBy1 intP0 (strP ",") <* newline
  op <- sc *> strP "Operation: new =" *> opP <* newline
  iCond <- sc *> strP "Test: divisible by" *> intP0 <* newline
  iTrue <- sc *> strP "If true: throw to monkey" *> intP0 <* newline
  iFalse <- sc *> strP "If false: throw to monkey" *> intP0 <* newline
  pure $ Monkey id items op (Test iCond iTrue iFalse) 0

opP :: Parser Op
opP = do
  op <- argP *> (strP "+" <|> strP "*")
  case op of
    "+" -> Add <$> argP
    _ -> Mul <$> argP

argP :: Parser Arg
argP = do
  argS <- strP "old" <|> some digitChar
  maybeToP parseArg argS
  where
    parseArg :: String -> Maybe Arg
    parseArg = \case
      "old" -> Just Old
      s -> Just . Int =<< stringToInt s
