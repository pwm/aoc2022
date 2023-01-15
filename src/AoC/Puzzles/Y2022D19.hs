module AoC.Puzzles.Y2022D19 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id)
import Control.Monad.Logic
import Data.Set qualified as Set

parse :: String -> Maybe [BP]
parse = parseMaybe (sepEndBy1 bpP newline)

solveA :: [BP] -> Int
solveA = sum . map ((\s -> s.bp.id * s.best.minerals.geode) . solve 24)

solveB :: [BP] -> Int
solveB = product . map ((\s -> s.best.minerals.geode) . solve 32) . take 3

data Candidate = Candidate
  { t :: Int,
    robots :: Robots,
    minerals :: Minerals
  }
  deriving stock (Show, Eq, Ord, Generic)

data Search = Search
  { tLimit :: Int,
    bp :: BP,
    best :: Candidate,
    seen :: Set Candidate,
    iters :: Int,
    prunes :: Int,
    samplingFreq :: Int
  }
  deriving stock (Show, Eq, Generic)

solve :: Int -> BP -> Search
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

initSearch :: Int -> BP -> Search
initSearch tLimit bp =
  Search
    { tLimit = tLimit,
      bp = bp,
      best = initCandidate,
      seen = mempty,
      iters = 0,
      prunes = 0,
      samplingFreq = 1e6
    }

{-
TODO:
 - throw away excess minerals/robot so that the seen set in much smaller and gets hit more often
 - batch time instead of 1-by-1
 - A state at time T can never be more valuable if the projected value for any kind of material is less than/equal to the best
-}

solver :: Candidate -> LogicT (State Search) ()
solver c = do
  s <- get
  when (s.iters `mod` s.samplingFreq == 0) $
    traceM ("ITER " <> rpad 9 (show s.iters) <> " | PRUNES " <> rpad 9 (show s.prunes))
  #iters += 1
  if
      | s.iters == 1e8 -> error "Too many iterations, aborting ..."
      | c.t >= s.tLimit -> error "Turn overflow ..."
      -- no need to run the last tree level, can check in the penultimate round for best
      | c.t == s.tLimit - 1,
        c.minerals.geode + (mine c.robots).geode > s.best.minerals.geode ->
          #best .= (c & #minerals <>~ mine c.robots)
      | c.t == s.tLimit - 1 -> empty
      -- if we could build geode robots in all remaining turns and still have less or equal
      | maxGeodeBound s.tLimit c <= s.best.robots.rGeode -> #prunes += 1 >> empty
      | Set.member c s.seen -> #prunes += 1 >> empty
      | otherwise -> do
          #seen %= Set.insert c
          (newRobots, buildCost) <- choose $ build s.bp c
          solver $
            c
              & #t +~ 1
              & #minerals <>~ (mine c.robots <> buildCost)
              & #robots <>~ newRobots

maxGeodeBound :: Int -> Candidate -> Int
maxGeodeBound tLimit c = c.robots.rGeode + tLimit - c.t

mine :: Robots -> Minerals
mine rs = Minerals rs.rOre rs.rClay rs.rObsidian rs.rGeode

{-
- don't build more ore robot than the ore cost of the most expensive robot
  note: doing the same rule for clay/obsidian seems to not matter much
- if we can build all then always build something
-}
build :: BP -> Candidate -> [(Robots, Minerals)]
build bp c
  | Just (rs, cost) <- mkGeode bp c.minerals = [(rs, cost)] -- unsafe assumption?
  | c.robots.rOre >= bp.maxOre = addNoneIf ((/= 2) . length) $ tryBuildAll [mkClay, mkObsidian]
  | otherwise = addNoneIf ((/= 3) . length) $ tryBuildAll [mkOre, mkClay, mkObsidian]
  where
    tryBuildAll = mapMaybe (\builder -> builder bp c.minerals)
    addNoneIf p xs = if p xs then xs <> [(mempty, mempty)] else xs

mkGeode, mkObsidian, mkClay, mkOre :: BP -> Minerals -> Maybe (Robots, Minerals)
mkGeode (view #cGeode -> CostGeode ore obsidian) ms
  | ms.ore >= ore && ms.obsidian >= obsidian =
      Just (mempty & #rGeode +~ 1, mempty & #ore -~ ore & #obsidian -~ obsidian)
  | otherwise = Nothing
mkObsidian (view #cObsidian -> CostObsidian ore clay) ms
  | ms.ore >= ore && ms.clay >= clay =
      Just (mempty & #rObsidian +~ 1, mempty & #ore -~ ore & #clay -~ clay)
  | otherwise = Nothing
mkClay (view #cClay -> CostClay ore) ms
  | ms.ore >= ore = Just (mempty & #rClay +~ 1, mempty & #ore -~ ore)
  | otherwise = Nothing
mkOre (view #cOre -> CostOre ore) ms
  | ms.ore >= ore = Just (mempty & #rOre +~ 1, mempty & #ore -~ ore)
  | otherwise = Nothing

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

data BP = BP
  { id :: Int,
    cOre :: CostOre,
    cClay :: CostClay,
    cObsidian :: CostObsidian,
    cGeode :: CostGeode,
    maxOre :: Int
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

bpP :: Parser BP
bpP = do
  id <- strP "Blueprint" *> intP <* strP ":"
  oreOre <- strP "Each ore robot costs" *> intP <* strP "ore."
  clayOre <- strP "Each clay robot costs" *> intP <* strP "ore."
  obsidianOre <- strP "Each obsidian robot costs" *> intP <* strP "ore and"
  obsidianClay <- intP <* strP "clay."
  geodeOre <- strP "Each geode robot costs" *> intP <* strP "ore and"
  geodeObsidian <- intP <* strP "obsidian."
  pure $
    BP
      { id,
        cOre = CostOre oreOre,
        cClay = CostClay clayOre,
        cObsidian = CostObsidian obsidianOre obsidianClay,
        cGeode = CostGeode geodeOre geodeObsidian,
        maxOre = maximum [oreOre, clayOre, obsidianOre, geodeOre]
      }
