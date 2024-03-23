module Main where

import Data.List (sortOn)
import qualified Data.Array as A
import qualified Data.Set as S

type Grid = A.Array (Int, Int) Int

data State = State{ currPos  :: (Int, Int)
                  , prevPos  :: (Int, Int)
                  , reps     :: Int
                  } deriving (Eq, Ord, Show)

data Node = Node{ state    :: State
                , heatLoss :: Int
                } deriving (Show)

-- Parsing input file --

parseData :: String -> Grid
parseData xs = A.listArray ((1, 1),(length rows, length $ head rows)) tiles
  where
    rows = lines xs
    tiles = map (read . (:[])) $ concat rows

-- Task 1 --

-- Produce all states accessible from a given state respecting constraints
-- on operator usage
expandState :: (Int, Int) -> State -> [State]
expandState (cmin, cmax) (State st@(x, y) (prevX, prevY) n)
  | n >= cmax = map mkState [(leftPos, 1), (rightPos, 1)]
  | n < cmin  = map mkState [(straightPos, n + 1)]
  | otherwise = map mkState [(leftPos, 1), (rightPos, 1), (straightPos, n + 1)]
  where
    leftPos     = (x - (y - prevY), y + (x - prevX))
    rightPos    = (x + (y - prevY), y - (x - prevX))
    straightPos = (x + (x - prevX), y + (y - prevY))
    mkState (pos, r) = State pos st r

-- Generate all valid nodes following from a given node
nextNodes :: Grid -> ((Int, Int), (Int, Int)) -> (Int, Int) -> Node -> [Node]
nextNodes grid bounds constr (Node state hl) = map makeNode states
  where
    states = filter (A.inRange bounds . currPos) $ expandState constr state
    makeNode state = Node state (hl + (grid A.! currPos state))

-- Prepare initial nodes to start search
initOpen :: Grid -> [Node]
initOpen grid = [ Node (State (1,2) (1,1) 1) (grid A.! (1,2)),
                  Node (State (2,1) (1,1) 1) (grid A.! (2,1))]

-- Uniform cost search of state space
search :: Grid-> ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> [Node] -> S.Set State -> Int
search grid bounds constr goal (node@(Node st hl) : open) close
  | (currPos $ state node) == goal = hl
  | st `S.member` close = search grid bounds constr goal open close
  | otherwise = search grid bounds constr goal newOpen newClose
  where
    expandedNodes = nextNodes grid bounds constr node
    newOpen = sortOn heatLoss $ expandedNodes <> open
    newClose = S.insert st close

-- Prepare and start state space search
findLength :: (Int, Int) -> Grid -> Int
findLength constr grid =
  search grid bounds constr (snd bounds) (initOpen grid) S.empty
  where
    bounds = A.bounds grid

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
