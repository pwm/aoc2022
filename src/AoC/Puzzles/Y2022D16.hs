module AoC.Puzzles.Y2022D16 where

import AoC.Lib.Graph
import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id)
import Control.Monad.Logic
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

{-
Legendary AoC puzzle :)

1. Shrink the graph to working valves only (now with edge weights)
   and keep shrinking it as we search it.

2. Branch and bound for part A where the bound is the following:
   If we could open all remaining vales in the next round and would
   still score lower than the current max

3. For part B run the search now without the bound (otherwise we'll miss combinations)
   and keep a max score for all combination of open valves. Then get the best score by
   finding the maximum score of 2 disjoint sets of valve combination scores.

4. A big optimisation for part B is to use a bit set (in an IntMap) for combinations.
-}

parse :: String -> Maybe G
parse = fmap Map.fromList . parseMaybe (sepEndBy1 lineP newline)

solveA :: G -> Int
solveA g = (solve 30 True "AA" g).best

solveB :: G -> Int
solveB g =
  -- This assumes players work till the end (ie. doesn't work on the sample input)
  let solves = IntMap.toList (solve 26 False "AA" g).solves
      scores = [x + y | (s1, x) <- solves, (s2, y) <- solves, disjoint s1 s2]
   in maximumOr 0 scores

type G = Map String (Int, [String])

data Valve = Valve
  { id :: Int,
    flow :: Int,
    dist :: Int
  }
  deriving stock (Show, Generic)

type Paths = IntMap [Valve]

data Candidate = Candidate
  { t :: Int,
    paths :: Paths,
    nexts :: [Valve],
    opens :: [Valve],
    score :: Int
  }
  deriving stock (Show, Generic)

data Search = Search
  { tLimit :: Int,
    prune :: Bool,
    best :: Int,
    solves :: IntMap Int
  }
  deriving stock (Show, Generic)

solve :: Int -> Bool -> String -> G -> Search
solve tLimit prune start g =
  let c = initCandidate start g
      s = Search tLimit prune 0 mempty
   in execState (observeAllT (solver c)) s

initCandidate :: String -> G -> Candidate
initCandidate start g =
  let paths0 = calcPaths start g
   in Candidate
        { t = 1,
          paths = rmValve 0 paths0,
          nexts = fromMaybe [] (paths0 IntMap.!? 0),
          opens = [],
          score = 0
        }

-- Compress the graph to working valves only + AA with egde weights (ie. distances)
-- Use powers of 2 for valve ids so we can store sets of them in a BitSet
calcPaths :: String -> G -> Paths
calcPaths start g =
  foldr mkPaths mempty [(from, to) | from <- start : vids, to <- start : vids, from /= to]
  where
    vids = [vid | (vid, (flow, _)) <- Map.toList g, flow > 0]
    bitMap = Map.fromList $ (start, 0) : zip vids [2 ^ n | n <- [1 :: Int ..]]
    mkPaths :: (String, String) -> Paths -> Paths
    mkPaths (from, to) =
      let valve = Valve (bitMap ! to) (fst (g ! to)) (calcDist from to)
       in IntMap.insertWith (<>) (bitMap ! from) [valve]
    calcDist :: String -> String -> Int
    calcDist from to = length (bfsSP (snd . (g !)) to from) - 1

solver :: Candidate -> LogicT (State Search) ()
solver c = do
  s <- get
  let tLeft = s.tLimit - c.t
  let opensBitSet = sum $ map (\v -> v.id) c.opens
  put s {solves = IntMap.insertWith max opensBitSet c.score s.solves}
  if
      | c.t == s.tLimit -> #best .= c.score `max` s.best
      | null c.nexts -> solver $ tick tLeft c
      -- If we could open all remaining vales in the next round and would
      -- still score lower than the current max then abandon this branch
      -- Only use it for part A as for part B we need all valve states
      | s.prune && upperBound tLeft c <= s.best -> empty
      | otherwise -> do
          to <- choose c.nexts
          if tLeft < to.dist + 1
            then solver $ tick tLeft c
            else solver $ openValve to $ tick to.dist c

upperBound :: Int -> Candidate -> Int
upperBound tLeft c =
  c.score
    + tLeft * flows c.opens
    + (tLeft - 1) * sum [v.flow | v <- c.nexts, v.dist < tLeft]

tick :: Int -> Candidate -> Candidate
tick dist c = c {t = c.t + dist, score = c.score + dist * flows c.opens}

openValve :: Valve -> Candidate -> Candidate
openValve v c =
  c
    { t = c.t + 1,
      paths = rmValve v.id c.paths,
      nexts = c.paths IntMap.! v.id,
      opens = v : c.opens,
      score = c.score + flows c.opens + v.flow
    }

flows :: [Valve] -> Int
flows = sum . map (\v -> v.flow)

rmValve :: Int -> Paths -> Paths
rmValve vid = IntMap.delete vid . IntMap.map (filter (\v -> v.id /= vid))

disjoint :: (Bits a, Num a) => a -> a -> Bool
a `disjoint` b = a .&. b == 0

lineP :: Parser (String, (Int, [String]))
lineP = do
  vid <- strP "Valve" *> some upperChar <* strP " has flow rate="
  flow <- intP <* strP ";"
  _ <- pluralStrP "tunnel" <* pluralStrP "lead" <* strP "to" <* pluralStrP "valve"
  neighbours <- sepBy1 (some upperChar) (strP ",")
  pure (vid, (flow, neighbours))
  where
    pluralStrP :: String -> Parser String
    pluralStrP s = strP (s <> "s") <|> strP s
