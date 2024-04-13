module Main where

import Data.Char (isAlpha)
import qualified Data.Map as Map
import qualified Queue as Q
import Text.ParserCombinators.ReadP

data Pulse = Low | High deriving (Eq, Show)

type NodeName = String
type Destinations = [NodeName]
data ModuleType = FF | Conj | Broad | But | Out deriving (Eq, Show)
data NodeDef = Module{mt :: ModuleType, dest :: Destinations} deriving (Eq, Show)
type Nodes = Map.Map NodeName NodeDef

data InState = On | Off deriving (Eq, Show)
data NodeState = In InState | Mem (Map.Map NodeName Pulse) deriving (Eq, Show)
type NodeStates = Map.Map NodeName NodeState

type Msg = (NodeName, Pulse, NodeName)

-- Parsing input file --

nameP :: ReadP String
nameP = munch1 isAlpha

makeNode :: Char -> String -> [String] -> (NodeName, NodeDef)
makeNode '%' name names = (name, Module FF names)
makeNode '&' name names = (name, Module Conj names)
makeNode 'b' name names = ('b' : name, Module Broad names)

nodeP :: ReadP (NodeName, NodeDef)
nodeP =
  makeNode <$> get <*> nameP <* string " -> " <*> nameP `sepBy1` string ", "

nodesP :: ReadP [(NodeName, NodeDef)]
nodesP = nodeP `endBy1` char '\n' <* eof

parseData :: String -> Nodes
parseData text = Map.fromList (specialNodes ++ inOutNodes)
  where
    inOutNodes = fst . head . readP_to_S nodesP $ text
    specialNodes = [("button", Module But ["broadcaster"])]
                    ++ map (, Module Out []) outNodes
    outNodes =
      filter (`notElem` map fst inOutNodes) $ concatMap (dest . snd) inOutNodes

-- Task 1 --

-- Set default initial nide states
initStates :: Nodes -> NodeStates
initStates nodeMap = Map.fromList $ concatMap initState nodes
  where
    nodes = Map.assocs nodeMap
    initState (name, Module FF _ ) = [(name, In Off)]
    initState (name, Module Conj _ ) =
      [(name, Mem (Map.fromList $ map (,Low) (callers name)))]
    initState (name, Module _ _ ) = []
    callers name = map fst $ filter (elem name . dest . snd) nodes

-- Input pulse processing by different node types - node state change and
-- output pulses generation
reactNode ::
  Nodes -> NodeStates -> NodeName -> ModuleType -> NodeName -> Pulse
  -> (NodeStates, [Msg])
reactNode net states name FF _ High = (states, [])
reactNode net states name FF _ Low =
  (Map.insert name (In newState) states, signals)
  where
    In st = states Map.! name
    newState = if st == On then Off else On
    outPulse = if newState == On then High else Low
    signals = map (name, outPulse,) . dest $ net Map.! name
reactNode net states name Conj from pulse =
  (Map.insert name (Mem newState) states, signals)
  where
    Mem state = states Map.! name
    newState = Map.insert from pulse state
    outPulse = if all (== High) (Map.elems newState) then Low else High
    signals = map (name, outPulse,) . dest $ net Map.! name
reactNode net states name Broad _ pulse = (states, signals)
  where
    signals = map (name, pulse,) . dest $ net Map.! name
reactNode _ states name Out _ _ = (states, [])

-- Passing pulses within node network initialized by a given message
react :: Nodes -> NodeStates -> Q.Queue Msg ->[Msg] -> ([Msg], NodeStates)
react net states queue signals
  | Q.isEmpty queue = (signals, states)
  | otherwise = react net newStates newQueue (sig : signals)
  where
    Just (sig@(from, pulse, to), rest) = Q.dequeue queue
    (newStates, newSignals) =
      reactNode net states to (mt $ net Map.! to) from pulse
    newQueue = foldl (\q e -> Q.enqueue e q) rest newSignals

-- Process one thousand repetitions of pushing button
thousandPresses :: Nodes -> Int
thousandPresses net =
  go net (initStates net) 1000 (0,0)
  where
    go net states 0 (l, h) = l * h
    go net states n (x, y) = go net newStates (n - 1) (x + l, y + h)
      where
        (signals, newStates) =
          react net states (Q.singleton ("button", Low, "broadcaster")) []
        l = length $ filter (\(_, x, _) -> x == Low) signals
        h = length $ filter (\(_, x, _) -> x == High) signals

task1 filename = thousandPresses . parseData <$> readFile filename

-- Task 2 --

-- Process repetitive button pushes until a given target node receives Low pulse
activationTime :: Nodes -> NodeName -> Int
activationTime net subgoal = go net (initStates net) 1
  where
    go net states n = if activated then n else go net newStates (n + 1)
      where
        (signals, newStates) =
          react net states (Q.singleton ("button", Low, "broadcaster")) []
        activated = any (\(_, x, y) -> x == Low && y == subgoal) signals

-- Calculate button push repetitions until node "rx" receives Low pulse
-- ("cheating" based on discovered properties of the given node net)
rxActivation :: Nodes -> Int
--rxActivation net = activationTime net "rx"
rxActivation net = product $ map (activationTime net) ["mr", "kk", "gl", "bb"]

task2 = rxActivation . parseData <$> readFile "day20-input"

-- Useful staff --

testTask1 =
  (\x y -> if x == 32000000 && y == 11687500
           then "OK" else "Something went wrong")
  <$> task1 "day20-example1"
  <*> task1 "day20-example2"

main = do
  x <- task1 "day20-input"
  y <- task2
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
