module AoC.Puzzles.Y2022D19 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id)
import Control.Monad.Logic
import Data.Set qualified as Set

parse :: String -> Maybe [Blueprint]
parse = parseMaybe (sepEndBy1 blueprintP newline)

-- 1294 (Runtime: 690420ms)
solveA :: [Blueprint] -> Int
solveA = sum . map ((\s -> s.blueprint.id * s.best.minerals.geode) . solve 24)

solveB :: [Blueprint] -> ()
solveB _ = ()

data Candidate = Candidate
  { t :: Int,
    robots :: Robots,
    minerals :: Minerals
  }
  deriving stock (Show, Eq, Ord, Generic)

data Search = Search
  { tLimit :: Int,
    blueprint :: Blueprint,
    best :: Candidate,
    seen :: Set Candidate,
    iters :: Int,
    prunes :: Int,
    samplingFreq :: Int
  }
  deriving stock (Show, Eq, Generic)

solve :: Int -> Blueprint -> Search
solve tLimit bp =
  let c = initCandidate
      s = initSearch tLimit bp
   in execState (observeAllT (solver c)) s

initCandidate :: Candidate
initCandidate =
  Candidate
    { t = 0,
      robots = mempty & #rOre +~ 1,
      minerals = mempty
    }

initSearch :: Int -> Blueprint -> Search
initSearch tLimit blueprint =
  Search
    { tLimit = tLimit,
      blueprint,
      best = initCandidate,
      seen = mempty,
      iters = 0,
      prunes = 0,
      samplingFreq = 1e6
    }

solver :: Candidate -> LogicT (State Search) ()
solver c = do
  s <- get
  when (s.iters `mod` s.samplingFreq == 0) $
    traceM ("ITER " <> rpad 9 (show s.iters) <> " | PRUNES " <> rpad 9 (show s.prunes))
  #iters += 1
  if
      | s.iters == 1e8 -> error "Too many iterations, aborting ..."
      | c.t > s.tLimit -> error "Turn overflow ..."
      | c.t == s.tLimit, c.minerals.geode > s.best.minerals.geode -> #best .= c
      | c.t == s.tLimit -> empty
      | Set.member c s.seen -> #prunes += 1 >> empty
      | otherwise -> do
          #seen %= Set.insert c
          (newRobots, buildCost) <- choose $ build s.blueprint c.minerals
          solver $
            c
              & #t +~ 1
              & #minerals <>~ (mine c.robots <> buildCost)
              & #robots <>~ newRobots

mine :: Robots -> Minerals
mine rs = Minerals rs.rOre rs.rClay rs.rObsidian rs.rGeode

build :: Blueprint -> Minerals -> [(Robots, Minerals)]
build bp ms = mapMaybe (\builder -> builder bp ms) [bGeode, bObsidian, bClay, bOre, bNone]

bGeode, bObsidian, bClay, bOre, bNone :: Blueprint -> Minerals -> Maybe (Robots, Minerals)
bGeode (view #cGeode -> CostGeode ore obsidian) ms
  | ms.ore >= ore && ms.obsidian >= obsidian =
      Just (mempty & #rGeode +~ 1, mempty & #ore -~ ore & #obsidian -~ obsidian)
  | otherwise = Nothing
bObsidian (view #cObsidian -> CostObsidian ore clay) ms
  | ms.ore >= ore && ms.clay >= clay =
      Just (mempty & #rObsidian +~ 1, mempty & #ore -~ ore & #clay -~ clay)
  | otherwise = Nothing
bClay (view #cClay -> CostClay ore) ms
  | ms.ore >= ore = Just (mempty & #rClay +~ 1, mempty & #ore -~ ore)
  | otherwise = Nothing
bOre (view #cOre -> CostOre ore) ms
  | ms.ore >= ore = Just (mempty & #rOre +~ 1, mempty & #ore -~ ore)
  | otherwise = Nothing
bNone _ _ = Just (mempty, mempty)

data Robots = Robots
  {rOre :: Int, rClay :: Int, rObsidian :: Int, rGeode :: Int}
  deriving stock (Show, Eq, Ord, Generic)

instance Semigroup Robots where
  (<>) :: Robots -> Robots -> Robots
  a <> b = Robots (a.rOre + b.rOre) (a.rClay + b.rClay) (a.rObsidian + b.rObsidian) (a.rGeode + b.rGeode)

instance Monoid Robots where
  mempty :: Robots
  mempty = Robots 0 0 0 0

data Minerals = Minerals
  {ore :: Int, clay :: Int, obsidian :: Int, geode :: Int}
  deriving stock (Show, Eq, Ord, Generic)

instance Semigroup Minerals where
  (<>) :: Minerals -> Minerals -> Minerals
  a <> b = Minerals (a.ore + b.ore) (a.clay + b.clay) (a.obsidian + b.obsidian) (a.geode + b.geode)

instance Monoid Minerals where
  mempty :: Minerals
  mempty = Minerals 0 0 0 0

data Blueprint = Blueprint
  { id :: Int,
    cOre :: CostOre,
    cClay :: CostClay,
    cObsidian :: CostObsidian,
    cGeode :: CostGeode
  }
  deriving stock (Show, Eq, Ord, Generic)

data CostOre = CostOre Int
  deriving stock (Show, Eq, Ord, Generic)

data CostClay = CostClay Int
  deriving stock (Show, Eq, Ord, Generic)

data CostObsidian = CostObsidian Int Int
  deriving stock (Show, Eq, Ord, Generic)

data CostGeode = CostGeode Int Int
  deriving stock (Show, Eq, Ord, Generic)

blueprintP :: Parser Blueprint
blueprintP = do
  id <- strP "Blueprint" *> intP <* strP ":"
  oreOre <- strP "Each ore robot costs" *> intP <* strP "ore."
  clayOre <- strP "Each clay robot costs" *> intP <* strP "ore."
  obsidianOre <- strP "Each obsidian robot costs" *> intP <* strP "ore and"
  obsidianClay <- intP <* strP "clay."
  geodeOre <- strP "Each geode robot costs" *> intP <* strP "ore and"
  geodeObsidian <- intP <* strP "obsidian."
  pure $
    Blueprint
      { id,
        cOre = CostOre oreOre,
        cClay = CostClay clayOre,
        cObsidian = CostObsidian obsidianOre obsidianClay,
        cGeode = CostGeode geodeOre geodeObsidian
      }

--

s0 :: String
s0 =
  unpack
    [trimming|
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
|]

bp0, bp1 :: Blueprint
bp0 = Blueprint {id = 1, cOre = CostOre 4, cClay = CostClay 2, cObsidian = CostObsidian 3 14, cGeode = CostGeode 2 7}
bp1 = Blueprint {id = 2, cOre = CostOre 2, cClay = CostClay 3, cObsidian = CostObsidian 3 8, cGeode = CostGeode 3 12}
