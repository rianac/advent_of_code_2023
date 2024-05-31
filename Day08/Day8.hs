module Main where

import Data.Maybe (fromJust)

type Node = (String, (String, String))

-- Parse input file --

removeChars :: String -> String -> String
removeChars forbiden = filter (not . (flip elem forbiden))

parseData :: String -> (String, [Node])
parseData input =
  let (plan, _ : _ : nodes) = span (/= '\n') input
      makeNode [a, b, c] = (a, (b, c))
  in (plan, map (makeNode . parseNodes) $ lines nodes)
  where parseNodes = words . removeChars "=(,)"

-- Task 1 --

-- Find all possible continuations from a given node
nextState :: String -> [Node] -> (String,String)
nextState state instructions = fromJust $ lookup state instructions

-- Length calculation for graph traversal following given step sequence
runStates :: (String -> Bool) -> String -> Int -> [Node] -> String -> Int
runStates f (s:steps) c instr x
  | f x = c
  | s == 'L' = runStates f steps (c + 1) instr (fst $ nextState x instr)
  | s == 'R' = runStates f steps (c + 1) instr (snd $ nextState x instr)
  | otherwise = error "Unknown direction"

-- Prepare conditions for graph traversal
followPlan :: (String, [Node]) -> Int
followPlan (plan, nodes) = runStates (=="ZZZ") (cycle plan) 0 nodes "AAA" 

task1 filename = followPlan . parseData <$> readFile filename

testTask1 :: IO ()
testTask1 = do
  res1 <- task1 "day8-example1"
  res2 <- task1 "day8-example2"
  case (res1,res2) of
    (2,6) -> putStrLn "OK"
    _     -> putStrLn "something went wrong"

-- Task 2 --

-- Prepare conditions for several traversals and calculate number of
-- steps for their coordinated finishing at the same time
followGhoust (plan, nodes) =
  let xs = map fst $ filter (\x -> (last . fst) x == 'A') nodes
  in foldl1 lcm $ map (runStates (\x -> last x == 'Z') (cycle plan) 0 nodes) xs

task2 filename = followGhoust . parseData <$> readFile filename

testTask2 :: IO ()
testTask2 = do
  res <- task2 "day8-example3"
  case res of
    6 -> putStrLn "OK"
    _ -> putStrLn "something went wrong"

main :: IO ()
main = do
  x <- task1 "day8-input"
  y <- task2 "day8-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y

