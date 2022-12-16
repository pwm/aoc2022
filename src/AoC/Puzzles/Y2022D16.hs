module AoC.Puzzles.Y2022D16 where

import AoC.Lib.Graph qualified as Graph
import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (id, some)
import Control.Monad.Logic
import Control.Monad.State.Strict
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Pipes
parse = fmap Map.fromList . parseMaybe (sepEndBy1 lineP newline)

solveA :: Pipes -> Int
solveA pipes =
  let (winner, iters, prunes) = solve pipes
   in withIO (putStrLn ("iters/prunes: " <> show (iters, prunes))) winner.released

solveB :: Pipes -> ()
solveB _ = ()

type Pipes = Map ValveId (Valve, [ValveId])

newtype ValveId = ValveId String
  deriving stock (Show, Eq, Ord, Generic)

data Valve = Valve
  { id :: ValveId,
    flow :: Int,
    status :: Status
  }
  deriving stock (Show, Eq, Ord, Generic)

data Status = Closed | Opened
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

data Candidate = Candidate
  { turn :: Int,
    pipes :: Pipes,
    released :: Int,
    curId :: ValveId,
    opens :: [(ValveId, Int)]
  }
  deriving stock (Show, Eq, Ord, Generic)

mkCandidate :: Pipes -> Candidate
mkCandidate pipes =
  Candidate
    { turn = 1,
      pipes = pipes,
      released = 0,
      curId = ValveId "AA",
      opens = []
    }

data Search = Search
  { maxTurn :: Int,
    paths :: Map ValveId [(ValveId, (Int, [ValveId]))],
    winning :: Candidate,
    -- seen :: Map (Int, Set ValveId) Int,
    -- stats
    iters :: Int,
    prunes :: Int,
    samplingFreq :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

mkSearch :: Pipes -> Search
mkSearch pipes =
  Search
    { maxTurn = 30,
      paths = calcPaths pipes,
      winning = mkCandidate pipes,
      -- seen = mempty,
      iters = 0,
      prunes = 0,
      samplingFreq = 1e4
    }

solve :: Pipes -> (Candidate, Int, Int)
solve pipes =
  let candidate0 = mkCandidate pipes
      search0 = mkSearch pipes
      result = execState (observeAllT (solver candidate0)) search0
   in (result.winning, result.iters, result.prunes)

solver :: Candidate -> LogicT (State Search) Candidate
solver candidate = do
  search <- get
  when (search.iters `mod` search.samplingFreq == 0) $
    traceM ("ITERS " <> rpad 10 (show search.iters) <> " | PRUNES " <> show search.prunes)
  #iters += 1
  let turnsLeft = search.maxTurn - candidate.turn
  if
      | candidate.turn > search.maxTurn -> error ("Too many turns: " <> show candidate.turn)
      | candidate.turn == search.maxTurn,
        candidate.released > search.winning.released ->
          #winning .= candidate >> pure candidate
      | candidate.turn == search.maxTurn -> empty
      -- \| candidate.released < fromMaybe 0 (search.seen !? (candidate.turn, candidate.opens)) -> do
      --     #prunes += 1 >> empty
      | closeds <- getClosed candidate.pipes,
        null closeds ->
          solver $
            candidate
              & #turn +~ turnsLeft
              & #released +~ turnsLeft * curFlow candidate.pipes
      | otherwise -> do
          let nexts = filter (isOpen candidate.pipes . fst) (search.paths ! candidate.curId)
          (nextId, (dist, steps)) <- choose nexts
          let -- dist + 1 for open or turns left if there's no time to get to the next closed valve
              turnCost = min (dist + 1) turnsLeft
              -- have enough turns left to reach the next closed valve and open it
              canOpen = dist + 1 <= turnsLeft
              newFlow = if canOpen then (getById nextId candidate.pipes).flow else 0
              candidate' =
                candidate
                  & #pipes .~ (if canOpen then openValve nextId candidate.pipes else candidate.pipes)
                  & #curId .~ (if dist <= turnsLeft then nextId else candidate.curId)
                  & #turn .~ candidate.turn + turnCost
                  & #released .~ candidate.released + turnCost * curFlow candidate.pipes + newFlow
                  & #opens %~ (\l -> if canOpen then (nextId, candidate.turn + turnCost) : l else l)
          -- This has quite the cost, so the prune must worth it
          -- #seen %= Map.insertWith max (candidate'.turn, candidate'.opens) candidate'.released
          solver candidate'

calcPaths :: Pipes -> Map ValveId [(ValveId, (Int, [ValveId]))]
calcPaths pipes =
  let valveIds = Map.keys pipes
      pairs = filter (uncurry (/=)) $ liftA2 (,) valveIds valveIds
   in foldr (\(f, t) -> Map.insertWith (<>) f [(t, getPath f t)]) mempty pairs
  where
    getPath :: ValveId -> ValveId -> (Int, [ValveId])
    getPath from to =
      let vs = Graph.paths (\vid -> snd (pipes ! vid)) (== to) from
       in head $ sortOn fst $ map (\l -> (length l - 1, l)) vs

-- calcDists :: Pipes -> Map ValveId [(ValveId, Int)]
-- calcDists pipes =
--   let valveIds = Map.keys pipes
--       pairs = filter (uncurry (/=)) $ liftA2 (,) valveIds valveIds
--    in foldr (\(f, t) -> Map.insertWith (<>) f [(t, getDist f t)]) mempty pairs
--   where
--     getDist :: ValveId -> ValveId -> Int
--     getDist from to =
--       let vs = Graph.dijkstra (\vid -> map (,1) (snd (pipes ! vid))) (== to) from
--        in snd (last vs)

openValve :: ValveId -> Pipes -> Pipes
openValve = Map.adjust (first (set #status Opened))

curFlow :: Pipes -> Int
curFlow = sum . map (view (_1 . #flow)) . Map.elems . getOpened

getOpened, getClosed :: Pipes -> Pipes
getOpened = Map.filter ((== Opened) . view (_1 . #status))
getClosed = Map.filter ((== Closed) . view (_1 . #status))

isOpen :: Pipes -> ValveId -> Bool
isOpen pipes vid = (getById vid pipes).status == Closed

getById :: ValveId -> Pipes -> Valve
getById id pipes = fst (pipes ! id)

lineP :: Parser (ValveId, (Valve, [ValveId]))
lineP = do
  idStr <- strP "Valve" *> some upperChar <* strP " has flow rate="
  flow <- intP <* strP ";"
  _ <- strP "tunnels" <|> strP "tunnel"
  _ <- (strP "leads" <|> strP "lead") <* strP "to"
  _ <- strP "valves" <|> strP "valve"
  ids <- map ValveId <$> sepBy1 (some upperChar) (strP ",")
  let id = ValveId idStr
      status = if flow == 0 then Opened else Closed
  pure (id, (Valve {id, flow, status}, ids))

--

g0 :: Pipes
g0 =
  Map.fromList
    [ (ValveId "AA", (Valve {id = ValveId "AA", flow = 0, status = Opened}, [ValveId "DD", ValveId "II", ValveId "BB"])),
      (ValveId "BB", (Valve {id = ValveId "BB", flow = 13, status = Closed}, [ValveId "CC", ValveId "AA"])),
      (ValveId "CC", (Valve {id = ValveId "CC", flow = 2, status = Closed}, [ValveId "DD", ValveId "BB"])),
      (ValveId "DD", (Valve {id = ValveId "DD", flow = 20, status = Closed}, [ValveId "CC", ValveId "AA", ValveId "EE"])),
      (ValveId "EE", (Valve {id = ValveId "EE", flow = 3, status = Closed}, [ValveId "FF", ValveId "DD"])),
      (ValveId "FF", (Valve {id = ValveId "FF", flow = 0, status = Opened}, [ValveId "EE", ValveId "GG"])),
      (ValveId "GG", (Valve {id = ValveId "GG", flow = 0, status = Opened}, [ValveId "FF", ValveId "HH"])),
      (ValveId "HH", (Valve {id = ValveId "HH", flow = 22, status = Closed}, [ValveId "GG"])),
      (ValveId "II", (Valve {id = ValveId "II", flow = 0, status = Opened}, [ValveId "AA", ValveId "JJ"])),
      (ValveId "JJ", (Valve {id = ValveId "JJ", flow = 21, status = Closed}, [ValveId "II"]))
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
