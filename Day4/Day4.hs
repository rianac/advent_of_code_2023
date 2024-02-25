{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isSpace)
import Data.List (intersect)

data Card = Card { getId :: Int, getWinNumbers :: [Int], getNumbers :: [Int]}
  deriving Show

-- Parse all lines

cardIdP :: ReadP String
cardIdP = string "Card" >> munch1 isSpace *> munch isDigit <* char ':'

numbersP :: ReadP [String]
numbersP = munch isSpace >> sepBy (munch1 isDigit) (munch1 isSpace)

lineP :: ReadP Card
lineP = do
  card <- cardIdP
  wins <- numbersP
  string " |"
  nums <- numbersP
  eof
  return $ Card (read card) (map read wins) (map read nums)

parseCards :: String -> [Card]
parseCards =
  map (fst . head . readP_to_S lineP) . lines

{- Task 1:
Cards are valued based on number of successful guesses (1 point for the first
match, then each guess doubles the number of points)
-}

-- Calculate number of successful matches for one card
countMatches :: Card -> Int
countMatches =
  \Card{..} -> length $  getNumbers `intersect` getWinNumbers 

-- Calculates sum of card points
countHits :: [Card] -> Int
countHits =
  let valueHits = \x -> if x > 0 then 2 ^ (x-1) else 0
  in sum . map (valueHits . countMatches)

task1 :: FilePath -> IO Int
task1 filename =
  readFile filename >>= return . parseCards >>= return . countHits

testTask1 :: IO ()
testTask1 = do
  res <- task1 "day4-example"
  case res of
    13 -> putStrLn "OK"
    _  -> putStrLn "something went wrong"

{- Task 2:
Extend card deck by copying cards based on the number of their successful
matches and find the size of the final deck.
-}

-- merge copied cards to card stack
addCards :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
addCards _ _ [] = []
addCards _ 0 cards = cards
addCards number_added counter ((number_old,y):cards) =
  (number_old + number_added,y) : addCards number_added (counter-1) cards

-- count number of original and copied cards
countCards :: [Card] -> Int
countCards  =
  sum . map fst . copyCards . map (\x -> (1,countMatches x))
  where
    copyCards [] = []
    copyCards ((x,y):cards) =
      (x,y) : copyCards (addCards x y cards)

task2 :: FilePath -> IO Int
task2 filename =
  readFile filename >>= return . parseCards >>= return . countCards

testTask2 :: IO ()
testTask2 = do
  res <- task2 "day4-example"
  case res of
    30 -> putStrLn "OK"
    _  -> putStrLn "something went wrong"

main :: IO ()
main = do
  x <- task1 "day4-input"
  y <- task2 "day4-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
