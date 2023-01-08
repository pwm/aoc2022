module AoC.Puzzles.Y2022D16 where

import AoC.Lib.Graph
import AoC.Lib.Parser
import AoC.Lib.Prelude
import Control.Monad.Logic
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe AdjList
parse = fmap Map.fromList . parseMaybe (sepEndBy1 lineP newline)

solveA :: AdjList -> Int
solveA g = (solve 30 "AA" g).best.released

solveB :: AdjList -> Int
solveB g =
  let s26 = solve 26 "AA" g
      -- assuming the graph is big enough that player 1 can't finish it
      -- ie. does not work for the small sample graph
      jointBests =
        [ p1Best + (solve 26 "AA" (zeroValves g p1Valves)).best.released
          | (p1Valves, p1Best) <-
              Map.toList
                . dedupValves
                -- One player will score more or equal than the other
                -- so here we say that player 1 will be that player
                . Map.filter (>= s26.best.released `div` 2)
                $ s26.solves
        ]
   in maximumOr 0 jointBests

-- The idea is to remove opened sets when we also have a subset with a higher released value
-- This way we leave more options for player 2 to explore possibly scoring higher
dedupValves :: Map (Set String) Int -> Map (Set String) Int
dedupValves solves =
  foldr dedup mempty . sortOn (Set.size . fst) . Map.toList $ solves
  where
    dedup :: (Set String, Int) -> Map (Set String) Int -> Map (Set String) Int
    dedup (vSet, released) m =
      let isSuperWithLower s = Set.isSubsetOf vSet s && solves ! vSet >= solves ! s
          supers = Set.filter isSuperWithLower (Map.keysSet solves)
       in Map.insert vSet released (Map.withoutKeys m supers)

zeroValves :: AdjList -> Set String -> AdjList
zeroValves = Set.foldr (Map.adjust (first (const 0)))

type AdjList = Map String (Int, [String])

data Valve = Valve
  { name :: String,
    flow :: Int,
    dist :: Int
  }
  deriving stock (Show, Generic)

type Paths = Map String [Valve]

data Candidate = Candidate
  { t :: Int,
    paths :: Paths,
    nexts :: [Valve],
    opens :: [Valve],
    released :: Int
  }
  deriving stock (Show, Generic)

data Search = Search
  { tLimit :: Int,
    best :: Candidate,
    solves :: Map (Set String) Int
  }
  deriving stock (Show, Generic)

solve :: Int -> String -> AdjList -> Search
solve tLimit start g =
  let c = initCandidate start g
      s = Search tLimit c mempty
   in execState (observeAllT (solver c)) s

initCandidate :: String -> AdjList -> Candidate
initCandidate start g =
  let paths0 = calcPaths start g
   in Candidate
        { t = 1,
          paths = rmValve start paths0,
          nexts = fromMaybe [] (paths0 !? start),
          opens = [],
          released = 0
        }

-- Compress the graph to working valves only + AA
-- This also means we now need egde weights (ie. distances)
calcPaths :: String -> AdjList -> Paths
calcPaths start g =
  let vids = [vid | (vid, (flow, _)) <- Map.toList g, flow > 0 || vid == start]
   in foldr mkPaths mempty [(from, to) | from <- vids, to <- vids, from /= to]
  where
    mkPaths :: (String, String) -> Paths -> Paths
    mkPaths (from, to) = Map.insertWith (<>) from [Valve to (fst (g ! to)) (calcDist from to)]
    calcDist :: String -> String -> Int
    calcDist from to = length (bfsSP (snd . (g !)) to from) - 1

solver :: Candidate -> LogicT (State Search) ()
solver c = do
  s <- get
  let tLeft = s.tLimit - c.t
  if
      | c.t == s.tLimit -> do
          #best .= if c.released > s.best.released then c else s.best
          let opensSet = Set.fromList $ map (\v -> v.name) c.opens
          #solves .= Map.insertWith max opensSet c.released s.solves
      -- If we could open all remaining vales in the next round and would
      -- still score lower than the current max then abandon this branch
      | releasedUpperBound tLeft c <= s.best.released -> empty
      | null c.nexts -> solver $ tick tLeft c
      | otherwise -> do
          to <- choose c.nexts
          if tLeft < to.dist + 1
            then solver $ tick tLeft c
            else solver $ openValve to $ tick to.dist c

releasedUpperBound :: Int -> Candidate -> Int
releasedUpperBound tLeft c =
  c.released
    + tLeft * curFlow c.opens
    + (tLeft - 1) * sum [v.flow | v <- c.nexts, v.dist < tLeft]

tick :: Int -> Candidate -> Candidate
tick dist c =
  c
    { t = c.t + dist,
      released = c.released + dist * curFlow c.opens
    }

openValve :: Valve -> Candidate -> Candidate
openValve v c =
  c
    { t = c.t + 1,
      paths = rmValve v.name c.paths,
      nexts = c.paths ! v.name,
      opens = v : c.opens,
      released = c.released + curFlow c.opens + v.flow
    }

curFlow :: [Valve] -> Int
curFlow = sum . map (\v -> v.flow)

rmValve :: String -> Paths -> Paths
rmValve vid = Map.delete vid . Map.map (filter (\v -> v.name /= vid))

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
