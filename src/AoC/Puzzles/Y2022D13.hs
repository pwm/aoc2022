module AoC.Puzzles.Y2022D13 where

import AoC.Lib.Parser
import AoC.Lib.Prelude

parse :: String -> Maybe [(Packet, Packet)]
parse = parseMaybe (sepEndBy1 packetPairP newline)

solveA :: [(Packet, Packet)] -> Int
solveA =
  sum
    . map fst
    . filter (uncurry (<) . snd)
    . zip [1 ..]

solveB :: [(Packet, Packet)] -> Int
solveB =
  let findIdx :: Packet -> [(Int, Packet)] -> Int
      findIdx p = maybe 0 fst . find ((== p) . snd)
      dp2, dp6 :: Packet
      dp2 = Seq [Seq [Lit 2]]
      dp6 = Seq [Seq [Lit 6]]
   in product
        . t2l
        . (findIdx dp2 &&& findIdx dp6)
        . zip [1 ..]
        . sort
        . uncurry (<>)
        . unzip
        . ((dp2, dp6) :)

data Packet
  = Lit Int
  | Seq [Packet]
  deriving stock (Show, Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Lit l) (Lit r) = compare l r
  compare (Seq []) (Seq []) = EQ
  compare (Seq []) (Seq _) = LT
  compare (Seq _) (Seq []) = GT
  compare (Lit l) rt = compare (Seq [Lit l]) rt
  compare lt (Lit r) = compare lt (Seq [Lit r])
  compare (Seq (l : lt)) (Seq (r : rt))
    | res <- compare l r, res /= EQ = res
    | otherwise = compare (Seq lt) (Seq rt)

packetPairP :: Parser (Packet, Packet)
packetPairP = liftA2 (,) (packetP <* newline) (packetP <* newline)

packetP, litP, seqP :: Parser Packet
packetP = litP <|> seqP
litP = Lit <$> intP0
seqP = Seq <$> squareBracketsP0 (sepBy packetP (char ','))
