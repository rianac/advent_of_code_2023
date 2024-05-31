module Main where

import Text.Regex.TDFA ((=~), getAllTextMatches)

type History = [Int]

-- Parse input file --

parseData :: String -> [History]
parseData = map parseLine . lines
  where
    parseLine input = map read $ getAllTextMatches (input =~ "[-]*[0-9]+")

-- Task 1 --

-- Extend a sequence with one next number
makePrediction :: History -> Int
makePrediction = sum . map last . takeWhile (any (/=0)) . iterate differences
  where
    differences xs = zipWith (-) (tail xs) xs

-- Combine all predictions
predictions :: [History] -> Int
predictions xs = sum $ map makePrediction xs

task1 filename = predictions . parseData <$> readFile filename

-- Task 2 --

task2 filename = predictions . map reverse . parseData <$> readFile filename

-- Usefull staff --

testTask1 =
  (\x -> if x == 114 then "OK" else "Something went wrong")
    <$> task1 "day9-example"

testTask2 =
  (\x -> if x == 2 then "OK" else "Something went wrong")
    <$> task2 "day9-example"

main :: IO ()
main = do
  x <- task1 "day9-input"
  y <- task2 "day9-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
