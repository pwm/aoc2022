module AoC.Puzzles.Y2022D19 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id)
import Control.Monad.Logic
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe [Blueprint]
parse = parseMaybe (sepEndBy1 blueprintP newline)

solveA :: [Blueprint] -> Int
solveA _bp =
  let (winner, iters, prunes) = solve bp0
   in withIO (putStrLn ("iters/prunes: " <> show (iters, prunes))) winner.minerals.geode

solveB :: [Blueprint] -> ()
solveB _ = ()

qualityLevel :: Search -> Int
qualityLevel search = search.blueprint.id * search.winning.minerals.geode

data Candidate = Candidate
  { turn :: Int,
    robots :: Robots,
    minerals :: Minerals
  }
  deriving stock (Show, Eq, Ord, Generic)

mkCandidate :: Candidate
mkCandidate =
  Candidate
    { turn = 0,
      robots = mempty & #oreRobot +~ 1,
      minerals = mempty
    }

data Search = Search
  { maxTurn :: Int,
    blueprint :: Blueprint,
    winning :: Candidate,
    seen :: Set Candidate,
    maxGeodeRobotForTurn :: Map Int Int, -- (turn, geodeRobot)
    iters :: Int,
    prunes :: Int,
    samplingFreq :: Int
  }
  deriving stock (Show, Eq, Generic)

mkSearch :: Blueprint -> Search
mkSearch blueprint =
  Search
    { maxTurn = 24,
      blueprint,
      winning = mkCandidate,
      seen = mempty,
      maxGeodeRobotForTurn = mempty,
      iters = 0,
      prunes = 0,
      samplingFreq = 1e3
    }

solve :: Blueprint -> (Candidate, Int, Int)
solve bp =
  let result = execState (observeAllT (solver mkCandidate)) (mkSearch bp)
   in (result.winning, result.iters, result.prunes)

{-
2 main ways to reduce state-space search:
 - prune branches that cannot produce better results
 - select most promising next candidates and eliminate bad candidates

prunes:
 - remember (robots, minerals) to only eval a position once
 - seen more geodes at this turn
 - choose best candidates

- goal is to maximise geodes
- to maximise geodes we need as many geode mining robots as possible as as soon as possible
- all robots compete for ore
-}

solver :: Candidate -> LogicT (State Search) Candidate
solver candidate = do
  search <- get
  when (search.iters `mod` search.samplingFreq == 0) $
    traceM ("ITER " <> rpad 10 (show search.iters) <> " | PRUNES " <> show search.prunes)
  #iters += 1
  if
      -- Assertions
      | search.iters == 1e7 -> error "Too many iterations ..."
      | candidate.minerals.ore >= 100 -> error "Too much ore ... "
      | candidate.turn > search.maxTurn -> error "Turn overflow ..."
      -- End states
      | candidate.turn == search.maxTurn && candidate.minerals.geode > search.winning.minerals.geode -> do
          traceM ("SUCCESS (iter: " <> show search.iters <> ")")
          #winning .= candidate
          pure candidate
      | candidate.turn == search.maxTurn -> do
          when (search.iters `mod` search.samplingFreq == 0) $
            traceM ("FAILURE (iter: " <> show search.iters <> ")")
          empty
      -- Prunes
      | Set.member candidate search.seen -> do
          #prunes += 1
          empty
      | maxGeodeRobotForTurn <- fromMaybe 0 (search.maxGeodeRobotForTurn !? candidate.turn),
        candidate.robots.geodeRobot < maxGeodeRobotForTurn -> do
          #prunes += 1
          empty
      -- Branching
      | otherwise -> do
          let choices = buildRobots search.blueprint candidate.minerals
          (reducedMinerals, newRobots) <- choose choices
          let robots' = candidate.robots <> newRobots
              minerals' = reducedMinerals <> mine candidate.robots
              candidate' =
                candidate
                  & #turn +~ 1
                  & #minerals .~ minerals'
                  & #robots .~ robots'
          -- todo: more/better memoisation to allow more prunes
          #seen %= Set.insert candidate
          #maxGeodeRobotForTurn %= Map.insertWith max candidate.turn candidate.robots.geodeRobot
          solver candidate'

{-
Assumptions:
 - if can build a geode build that(?)
 - if can build an obsidian build that(??)
 - otherwise build ore or clay or nothing

 Note: we don't select the "most built robots" but under normal operation
 we should only ever build 1 (is this true?)
-}
buildRobots :: Blueprint -> Minerals -> [(Minerals, Robots)]
buildRobots bp = nubOrd . observeAll . flip runStateT mempty . go
  where
    go :: Minerals -> StateT Robots Logic Minerals
    go ms
      | Just (rs, cost) <- buildGeode bp ms = modify (<> rs) >> go (ms <> cost)
      | Just (rs, cost) <- buildObsidian bp ms = modify (<> rs) >> go (ms <> cost)
      | otherwise = do
          -- note: builders order matter as search will descend left
          let builders = [buildClay, buildOre]
              builds = map (\builder -> builder bp ms) builders
          -- next being a Maybe allows to react to Nothing, ie. returning all minerals
          next <- choose builds
          case next of
            Just (rs, cost) -> modify (<> rs) >> go (ms <> cost)
            Nothing -> pure ms

buildGeode :: Blueprint -> Minerals -> Maybe (Robots, Minerals)
buildGeode (view #costGeodeRobot -> (CostGeode ore obsidian)) ms
  | ms.ore >= ore && ms.obsidian >= obsidian =
      Just (mempty & #geodeRobot +~ 1, mempty & #ore -~ ore & #obsidian -~ obsidian)
  | otherwise = Nothing

buildObsidian :: Blueprint -> Minerals -> Maybe (Robots, Minerals)
buildObsidian (view #costObsidianRobot -> (CostObsidian ore clay)) ms
  | ms.ore >= ore && ms.clay >= clay =
      Just (mempty & #obsidianRobot +~ 1, mempty & #ore -~ ore & #clay -~ clay)
  | otherwise = Nothing

buildClay :: Blueprint -> Minerals -> Maybe (Robots, Minerals)
buildClay (view #costClayRobot -> (CostClay ore)) ms
  | ms.ore >= ore = Just (mempty & #clayRobot +~ 1, mempty & #ore -~ ore)
  | otherwise = Nothing

buildOre :: Blueprint -> Minerals -> Maybe (Robots, Minerals)
buildOre (view #costOreRobot -> (CostOre ore)) ms
  | ms.ore >= ore = Just (mempty & #oreRobot +~ 1, mempty & #ore -~ ore)
  | otherwise = Nothing

mine :: Robots -> Minerals
mine rs = Minerals rs.oreRobot rs.clayRobot rs.obsidianRobot rs.geodeRobot

data Robots = Robots
  { oreRobot :: Int,
    clayRobot :: Int,
    obsidianRobot :: Int,
    geodeRobot :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Semigroup Robots where
  (<>) :: Robots -> Robots -> Robots
  a <> b =
    Robots
      { oreRobot = a.oreRobot + b.oreRobot,
        clayRobot = a.clayRobot + b.clayRobot,
        obsidianRobot = a.obsidianRobot + b.obsidianRobot,
        geodeRobot = a.geodeRobot + b.geodeRobot
      }

instance Monoid Robots where
  mempty :: Robots
  mempty = Robots 0 0 0 0

data Minerals = Minerals
  { ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geode :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Semigroup Minerals where
  (<>) :: Minerals -> Minerals -> Minerals
  a <> b =
    Minerals
      { ore = a.ore + b.ore,
        clay = a.clay + b.clay,
        obsidian = a.obsidian + b.obsidian,
        geode = a.geode + b.geode
      }

instance Monoid Minerals where
  mempty :: Minerals
  mempty = Minerals 0 0 0 0

data Blueprint = Blueprint
  { id :: Int,
    costOreRobot :: CostOre,
    costClayRobot :: CostClay,
    costObsidianRobot :: CostObsidian,
    costGeodeRobot :: CostGeode
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
        costOreRobot = CostOre oreOre,
        costClayRobot = CostClay clayOre,
        costObsidianRobot = CostObsidian obsidianOre obsidianClay,
        costGeodeRobot = CostGeode geodeOre geodeObsidian
      }

--

s0 :: String
s0 =
  unpack
    [trimming|
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
|]

{-
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.
-}
bp0, bp1 :: Blueprint
bp0 = Blueprint {id = 1, costOreRobot = CostOre 4, costClayRobot = CostClay 2, costObsidianRobot = CostObsidian 3 14, costGeodeRobot = CostGeode 2 7}
bp1 = Blueprint {id = 2, costOreRobot = CostOre 2, costClayRobot = CostClay 3, costObsidianRobot = CostObsidian 3 8, costGeodeRobot = CostGeode 3 12}
