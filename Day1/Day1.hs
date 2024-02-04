module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

{- Task 1:
The input document consists of lines of alphanumerical text. On each line,
the value can be found by combining the first digit and the last digit (in
that order) to form a single two-digit number. Consider the entire document,
what is the sum of all of the values?
-}

-- Combine first and last digit chars into two-digit number
combineDigits :: String -> Maybe Int
combineDigits line =
  let a = dropWhile (not . isDigit) line
      b = dropWhile (not . isDigit) $ reverse line
  in combine a b
  where
    combine "" "" = Nothing
    combine (x:_) (y:_) = Just $ read [x,y]

-- Sum of all two-digit numbers
makeSum :: [String] -> Maybe Int
makeSum lines =
  fmap sum $ sequence $ map combineDigits lines

task1 :: String -> IO (Maybe Int)
task1 filename =
  readFile filename >>= return . lines >>= return . makeSum

testTask1 :: IO ()
testTask1 =
  let total = sum [29, 83, 13, 24, 42, 14, 76]
  in do
       res <- task1 "day1-example1"
       case res of
         Just total -> putStrLn "OK"
         _ -> putStrLn "something went wrong"

{- Task 2:
To find the real first and last digit on each line, it is necessary
consider that some of the digits are actually spelled out with letters
(one, two, three, four, five, six, seven, eight, and nine). What is the
sum of all of the values?
-}

-- Allowed digit replacements
digits :: [(String,String)]
digits = [("zero","0"), ("one","1"), ("two","2"), ("three","3"), ("four","4"),
          ("five","5"), ("six","6"), ("seven","7"),("eight","8"), ("nine","9")]

-- Replace 'spelled digits' with appropriate digit characters
replaceDigits :: String -> String
replaceDigits [] = []
replaceDigits line  =
  case (findPattern line) of
    [] -> head line : replaceDigits (tail line)
    [(x,y)] -> y <> replaceDigits (drop (length x - 1) line)
  where
    findPattern line = filter (\x -> isPrefixOf (fst x) line) digits

task2 :: String -> IO (Maybe Int)
task2 filename =
  readFile filename >>= return . makeSum . lines . replaceDigits

testTask2 :: IO ()
testTask2 =
  let total = sum [29, 83, 13, 24, 42, 14, 76]
  in do
       res <- task2 "day1-example2"
       case res of
         Just total -> putStrLn "OK"
         _ -> putStrLn "something went wrong"

main :: IO ()
main =
  do
    x <- task1 "day1-input"
    y <- task2 "day1-input"
    putStrLn $ "Task 1: " ++ show x
    putStrLn $ "Task 2: " ++ show y

