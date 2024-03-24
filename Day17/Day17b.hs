module Main where

import Data.List (sortOn)
import qualified Data.Array as A
import qualified Data.Set as S


type Grid = A.Array (Int, Int) Int

data Direction = Vert | Horz deriving (Eq, Ord, Show)

data State = State{ pos  :: (Int, Int)
                  , dir  :: Direction
                  } deriving (Eq, Ord, Show)

data Node = Node{ state :: State
                , cost  :: Int
                } deriving (Eq, Show)

instance Ord Node where
  compare Node{state=s1, cost=c1} Node{state=s2, cost=c2}
    | c1 < c2 = LT
    | c1 > c2 = GT
    | otherwise = compare s1 s2

-- Parsing input file --

parseData :: String -> Grid
parseData xs = A.listArray ((1, 1),(length rows, length $ head rows)) tiles
  where
    rows = lines xs
    tiles = map (read . (:[])) $ concat rows

-- Task 1 --

-- Produce all states accessible from a given state respecting constraints
-- on operator usage
expandState :: (Int, Int) -> (Int, Int) -> State -> [State]
expandState (stepMin, stepMax) (maxX, maxY) (State (x, y) dir)
  | dir == Horz = map (\xNew -> State (xNew, y) Vert) $ movesUp <> movesDown
  | dir == Vert = map (\yNew -> State (x, yNew) Horz) $ movesLeft <> movesRight
  where
    movesUp    = [(max 1 $ x - stepMax) .. (x - stepMin)]
    movesDown  = [(x + stepMin) .. (min maxX $ x + stepMax)]
    movesLeft  = [(max 1 $ y - stepMax) .. (y - stepMin)]
    movesRight = [(y + stepMin) .. (min maxY $ y + stepMax)]

-- Generate all valid nodes following from a given node
nextNodes :: Grid -> (Int, Int) -> (Int, Int) -> Node -> [Node]
nextNodes grid constr bounds (Node state hl) = map makeNode states
  where
    states = expandState constr bounds state
    makeNode newState = Node newState (hl + costDelta grid state newState)

-- Calculate cost for a path through the grid
costDelta :: Grid -> State -> State -> Int
costDelta grid State{pos = (x, y)} State{pos = target@(xT, yT)} =
  sum . map (grid A.!) . A.range $ bounds
  where
    start = (x + signum (xT - x), y + signum (yT - y))
    bounds = if start < target then (start, target) else (target, start)

-- Prepare initial nodes to start search
initOpen :: S.Set Node
initOpen = S.fromList [ Node (State (1,1) Horz) 0, Node (State (1,1) Vert) 0]

-- Uniform cost search of state space
search :: Grid-> (Int, Int) -> (Int, Int) -> (Int, Int) -> S.Set Node -> S.Set State -> Int
search grid constr bounds goal open close
  | pos st == goal = hl
  | st `S.member` close = search grid constr bounds goal restOpen close
  | otherwise = search grid constr bounds goal newOpen newClose
  where
    (node@(Node st hl), restOpen) = S.deleteFindMin open
    expandedNodes = nextNodes grid constr bounds node
    newOpen = S.union restOpen $ S.fromList expandedNodes
    newClose = S.insert st close

-- Prepare and start state space search
findLength :: (Int, Int) -> Grid -> Int
findLength constr grid = search grid constr corner corner initOpen S.empty
  where
    corner = snd $ A.bounds grid

task1 filename = findLength (1,3) . parseData <$> readFile filename

-- Task 2 --

task2 filename = findLength (4,10) . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 102 then "OK" else "Something went wrong")
    <$> task1 "day17-example"

testTask2 =
  (\x -> if x == 94 then "OK" else "Something went wrong")
    <$> task2 "day17-example"

main = do
  x <- task1 "day17-input"
  y <- task2 "day17-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
