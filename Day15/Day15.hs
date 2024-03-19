module Main where

import Data.List.Split (splitOn, splitOneOf)
import Data.Map (Map)
import Data.Map as Map ((!), fromList, toList, insert)

type Lens = (String, Int)
type Box = [Lens]

-- Parse input data --

parseData :: String -> [String] 
parseData = splitOn "," . init

-- Task 1 --

-- Calculate hash value
hash :: String -> Int
hash = foldl (\h x -> (h + fromEnum x) * 17 `mod` 256) 0

-- Hash all commands
calculate :: [String] -> Int
calculate = sum . map hash

task1 filename = calculate . parseData <$> readFile filename

-- Task 2 --

-- Remove, replace or add lenses in a box
changeBox :: Box -> [String] -> Box
changeBox box [label, ""] = filter ((/= label) . fst) box
changeBox box [label, fl]
  | any ((== label) . fst) box =
      map (\(x,y) -> if x == label then (x, read fl) else (x,y)) box
  | otherwise = box <> [(label, read fl)]

-- Select an appropriate box and let it to be modified
updateBox :: Map Int Box -> String -> Map Int Box
updateBox boxes cmd = Map.insert box (changeBox (boxes ! box) [label, x]) boxes
  where
    [label, x] = splitOneOf "-=" cmd
    box = hash label

-- Calculate focusing power all lenses in all boxes
focusingPower :: Map Int Box -> Int
focusingPower = sum . map (\(x,y) -> (1+x) * boxPower y) . Map.toList
  where
    boxPower xs = sum $ zipWith (*) [1..] $ map snd xs

-- Install all lenses according to comands and evaluate all lenses
processCommands :: [String] -> Int
processCommands cmds =  focusingPower installedLenses 
  where
    emptyBoxes = Map.fromList $ zip [0..255] (repeat [])
    installedLenses = foldl updateBox emptyBoxes cmds

task2 filename = processCommands . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 1320 then "OK" else "Something went wrong")
    <$> task1 "day15-example"

testTask2 =
  (\x -> if x == 145 then "OK" else "Something went wrong")
    <$> task2 "day15-example"

main :: IO ()
main = do
  x <- task1 "day15-input"
  y <- task2 "day15-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
