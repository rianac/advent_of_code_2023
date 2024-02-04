module Day1 where

{- Task 1:
The input document consists of lines of alphanumerical text. On each line,
the value can be found by combining the first digit and the last digit (in
that order) to form a single two-digit number. Consider the entire document,
what is the sum of all of the values?
-}

findFirstDigit :: String -> Maybe Int
findFirstDigit [] = Nothing
findFirstDigit (x:xs)
  | elem x "0123456789" = Just $ read [x]
  | otherwise = findFirstDigit xs

combineDigits :: String -> Maybe Int
combineDigits line =
  let a = findFirstDigit line
      b = findFirstDigit $ reverse line
  in combine a b
  where
    combine (Just x) (Just y) = Just $ (10 * x) + y
    combine _ _ = Nothing

makeSum :: [String] -> Maybe Int
makeSum lines =
  let numbers = map combineDigits lines
  in foldl add (Just 0) numbers
  where 
    add (Just x) (Just y) = Just $ x + y
    add Nothing _ = Nothing
    add _ Nothing = Nothing

task1 :: String -> IO (Maybe Int)
task1 filename =
  readFile filename >>= return . words >>= return . makeSum

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

digits :: [(String,String)]
digits = [ ("zero","0"), ("one","1"), ("two","2"), ("three","3"),
           ("four","4"), ("five","5"), ("six","6"), ("seven","7"),
           ("eight","8"), ("nine","9") ]

findPattern :: String -> [(String,(String,Int))] -> Maybe (String, Int)
findPattern line patterns =
  case (line, patterns) of
    (_, []) -> Nothing
    (_, [ ("", x) ]) -> Just x
    ([], _) -> Nothing
    (x:xs, _) -> let candidates = filter (\y -> (head . fst) y == x) patterns
                 in findPattern xs $ map (\y -> ( (tail . fst) y, snd y)) candidates

replacePatterns :: String -> [(String,(String,Int))]-> String
replacePatterns [] _ = []
replacePatterns line replacements =
  case (findPattern line replacements) of
    Nothing -> head line : replacePatterns (tail line) replacements
    Just (x,y) -> x <> replacePatterns (drop (y-1) line) replacements

replaceDigits :: String -> String
replaceDigits line =
  let replacements = map (\x -> (fst x, (snd x, length (fst x)))) digits
  in replacePatterns line replacements

task2 :: String -> IO (Maybe Int)
task2 filename =
  do
    a <- readFile filename
    return (makeSum . words . replaceDigits $ a)

testTask2 :: IO ()
testTask2 =
  let total = sum [29, 83, 13, 24, 42, 14, 76]
  in do
       res <- task2 "day1-example2"
       case res of
         Just total -> putStrLn "OK"
         _ -> putStrLn "something went wrong"
