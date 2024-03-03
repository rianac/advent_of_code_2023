module Main where

import Data.Maybe (fromJust)
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map

type Graph = Map String (String, String)

-- Parse input file

removeChars :: String -> String -> String
removeChars forbiden = filter (not . (flip elem forbiden))

parseData :: String -> (String, Graph)
parseData input =
  let (plan, _ : _ : nodes) = span (/= '\n') input
      makeNode [a, b, c] = (a, (b, c))
  in (plan, Map.fromList $ map (makeNode . parseNodes) $ lines nodes)
  where parseNodes = words . removeChars "=(,)"

{- Task 1
To find the shortest path from node AAA to node ZZZ.
-}

-- Length calculation for graph traversal following given step sequence
runStates :: (String -> Bool) -> String -> Graph -> String -> Int
runStates f steps nodes start =
  length . takeWhile f $ scanl next start (cycle steps)
  where
    next state 'L' = fst $ nodes Map.! state
    next state 'R' = snd $ nodes Map.! state

-- Prepare conditions for graph traversal
followPlan :: (String, Graph) -> Int
followPlan (plan, nodes) = runStates (/="ZZZ") plan nodes "AAA"

task1 filename = followPlan . parseData <$> readFile filename

testTask1 :: IO ()
testTask1 = do
  res1 <- task1 "day8-example1"
  res2 <- task1 "day8-example2"
  case (res1,res2) of
    (2,6) -> putStrLn "OK"
    _     -> putStrLn "something went wrong"

{- Task 2
To find all shortest paths from nodes **A to nodes **Z such, that all have the
same length.
-}

-- Prepare conditions for several traversals and calculate number of
-- steps for their coordinated finishing at the same time
followGhoust :: (String, Graph) -> Int
followGhoust (plan, nodes) =
  let starts = filter ("A" `isSuffixOf`) $ Map.keys nodes
  in foldl1 lcm $ map (runStates (not . isSuffixOf "Z") plan nodes) starts

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
