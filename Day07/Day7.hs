module Main where

import Data.List       (group, sort, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe      (fromMaybe)

data Card
  = CardJoker |Card2 | Card3 | Card4 | Card5 | Card6 | Card7 | Card8
  | Card9 | CardT | CardJ | CardQ | CardK | CardA
  deriving (Show, Eq, Ord)

data HandType
  = High | OnePair | TwoPairs | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind
  deriving (Show, Eq, Ord)

data Hand = Hand HandType [Card] deriving (Show, Eq, Ord)

instance Read Hand where
  readsPrec _ [a, b, c, d, e] =
    let cards        = map findCard [a, b, c, d, e]
        cardsNoJoker = filter (/= CardJoker) cards
        hand_type    = findHand . sort . map length . group . sort $ cardsNoJoker
    in [(Hand hand_type cards, "")]
    where
      findCard x = fromMaybe (error "unknown card") $ lookup x card_map
      findHand x = case filter (elem x . fst) hand_map of
                     [(_,h)] -> h
                     _       -> error "unknown hand"
      card_map = [('A',CardA),('K',CardK),('Q',CardQ),('J',CardJ),('T',CardT),
                  ('9',Card9),('8',Card8),('7',Card7),('6',Card6),('5',Card5),
                  ('4',Card4),('3',Card3),('2',Card2),('*',CardJoker)]
      hand_map = [([[5], [4], [3], [2], [1], []],     FiveOfKind),
                  ([[1, 4], [1, 3], [1, 2], [1, 1]],  FourOfKind),
                  ([[2, 3], [2, 2]],                  FullHouse),
                  ([[1, 1, 3], [1, 1, 2], [1, 1, 1]], ThreeOfKind),
                  ([[1, 2, 2]],                       TwoPairs),
                  ([[1, 1, 1, 2], [1, 1, 1, 1]],      OnePair),
                  ([[1, 1, 1, 1, 1]],                 High)]

parseData :: String -> [(Hand, Int)]
parseData = map ((\[a, b] -> (read a, read b)) . words) . lines

-- Task 1 --

calculateWinning :: [(Hand, Int)] -> Int
calculateWinning = sum . map (\(r, (_, b)) -> r * b) . zip [1..] . sort

task1 filename = calculateWinning . parseData <$> readFile filename

testTask1 :: IO ()
testTask1 = do
  res <- task1 "day7-example"
  case res of
    6440 -> putStrLn "OK"
    _    -> putStrLn "something went wrong"

-- Task 2 --

replaceJ :: String -> String
replaceJ = intercalate "*" . splitOn "J"

task2 filename = calculateWinning . parseData . replaceJ <$> readFile filename

testTask2 :: IO ()
testTask2 = do
  res <- task2 "day7-example"
  case res of
    5905 -> putStrLn "OK"
    _    -> putStrLn "something went wrong"

main :: IO ()
main = do
  x <- task1 "day7-input"
  y <- task2 "day7-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
