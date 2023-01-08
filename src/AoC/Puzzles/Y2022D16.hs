module AoC.Puzzles.Y2022D16 where

import AoC.Lib.Graph
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Control.Monad.Logic
import Data.Map.Strict qualified as Map

parse :: String -> Maybe AdjList
parse = fmap Map.fromList . parseMaybe (sepEndBy1 lineP newline)

solveA :: AdjList -> Int
solveA adjList =
  let (winner, iters, prunes) = solve 30 "AA" adjList
      displayStats =
        putStrLn (ppsw 200 (winner {paths = mempty}))
          >> putStrLn ""
          >> putStrLn ("iters/prunes: " <> show (iters, prunes))
   in withIO displayStats winner.released

solveB :: AdjList -> ()
solveB _ = ()

type AdjList = Map String (Int, [String])

data Valve = Valve
  { name :: String,
    flow :: Int,
    dist :: Int
  }
  deriving stock (Show)

type Paths = Map String [Valve]

data Candidate = Candidate
  { t :: Int,
    paths :: Paths,
    nexts :: [Valve],
    opened :: [Valve],
    released :: Int
  }
  deriving stock (Show)

data Search = Search
  { tLimit :: Int,
    best :: Candidate,
    iters :: Int,
    prunes :: Int,
    samplingFreq :: Int
  }
  deriving stock (Show)

solve :: Int -> String -> AdjList -> (Candidate, Int, Int)
solve tLimit start adjList =
  let candidate = initCandidate start adjList
      search = Search tLimit candidate 0 0 1e4
      result = execState (observeAllT (solver candidate)) search
   in (result.best, result.iters, result.prunes)

initCandidate :: String -> AdjList -> Candidate
initCandidate start adjList =
  let paths0 = calcPaths start adjList
   in Candidate
        { t = 1,
          paths = removeValve start paths0,
          nexts = paths0 ! start,
          opened = [],
          released = 0
        }

calcPaths :: String -> AdjList -> Paths
calcPaths start adjList =
  let vids = [vid | (vid, (flow, _)) <- Map.toList adjList, flow > 0 || vid == start]
   in foldr mkPaths mempty [(from, to) | from <- vids, to <- vids, from /= to]
  where
    mkPaths :: (String, String) -> Paths -> Paths
    mkPaths (from, to) = Map.insertWith (<>) from [Valve to (fst (adjList ! to)) (calcDist from to)]
    calcDist :: String -> String -> Int
    calcDist from to = length (bfsSP (snd . (adjList !)) to from) - 1

solver :: Candidate -> LogicT (State Search) Candidate
solver c = do
  s <- get
  when (s.iters > 0 && s.iters `mod` s.samplingFreq == 0) $
    traceM ("ITERS " <> rpad 9 (show s.iters) <> " | PRUNES " <> rpad 9 (show s.prunes))
  put s {iters = s.iters + 1}
  let tLeft = s.tLimit - c.t
  if
      | c.t == s.tLimit, c.released > s.best.released -> put s {best = c} >> pure c
      | c.t == s.tLimit -> empty
      | releasedUpperBound tLeft c <= s.best.released -> put s {prunes = s.prunes + 1} >> empty
      | null c.nexts -> solver $ tick tLeft c
      | otherwise -> do
          to <- choose c.nexts
          if tLeft < to.dist + 1
            then solver $ tick tLeft c
            else solver $ openValve to $ tick to.dist c

releasedUpperBound :: Int -> Candidate -> Int
releasedUpperBound tLeft c =
  c.released
    + tLeft * curFlow c.opened
    + (tLeft - 1) * sum [v.flow | v <- c.nexts, v.dist < tLeft]

tick :: Int -> Candidate -> Candidate
tick dist c =
  c
    { t = c.t + dist,
      released = c.released + dist * curFlow c.opened
    }

openValve :: Valve -> Candidate -> Candidate
openValve v c =
  c
    { t = c.t + 1,
      paths = removeValve v.name c.paths,
      nexts = c.paths ! v.name,
      opened = v : c.opened,
      released = c.released + curFlow c.opened + v.flow
    }

curFlow :: [Valve] -> Int
curFlow = sum . map (\v -> v.flow)

removeValve :: String -> Paths -> Paths
removeValve vid = Map.delete vid . Map.map (filter (\v -> v.name /= vid))

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

--

g0 :: AdjList
g0 =
  Map.fromList
    [ ("AA", (0, ["DD", "II", "BB"])),
      ("BB", (13, ["CC", "AA"])),
      ("CC", (2, ["DD", "BB"])),
      ("DD", (20, ["CC", "AA", "EE"])),
      ("EE", (3, ["FF", "DD"])),
      ("FF", (0, ["EE", "GG"])),
      ("GG", (0, ["FF", "HH"])),
      ("HH", (22, ["GG"])),
      ("II", (0, ["AA", "JJ"])),
      ("JJ", (21, ["II"]))
    ]

s00 :: String
s00 =
  unpack
    [trimming|
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
|]
