module Main where

parseData :: String -> [[Int]]
parseData x = map (map read . drop 1 . words) $ lines x

{- Task 1:
Boat race - holding down a button for time 'h' charges the boat, releasing
button allows boat to move for remaining time. For each time unit charging
at the beginning, the boat speed increases by one unit.

A bit of analysis:
   t - available time,
   h - holding time (0 <= h <= t) as well as speed after holding time,
   d - distance (0 <= d) = (t-h)*h
       distance for holding h is the same as distance for holding (t-h)

r - previous record, hr - holding time for the record
     (t - hr) * hr = r
quadratic equation with two roots
     hr1 = ( t - sqrt( t^2 - 4*r ) ) / 2
     hr2 = ( t + sqrt( t^2 - 4*r ) ) / 2

h in {0..floor(h1)}          - worse than record
h in {ceil(hr1)..floor(hr2)} - better than record
h in {ceil(hr2),.t}          - worse than record
-}

newRecordRange :: (Int,Int) -> (Int,Int)
newRecordRange (t,r) =
  let (tf,rf) = (fromIntegral t, fromIntegral r)
      disc = sqrt $ tf**2 - 4*rf
      x = (tf - disc)/2
      y = (tf + disc)/2
  in case floor x == ceiling x of
       True  -> (ceiling x + 1,floor y - 1)
       False -> (ceiling x,floor y)

--findRangeProduct :: [[Int]] -> Int
findRangeProduct (x:y:[]) =
  product . map ((\(x,y) -> y-x+1) . newRecordRange) $ zip x y

task1 filename =
  findRangeProduct . parseData <$> readFile filename

testTask1 :: IO ()
testTask1 = do
  res <- task1 "day6-example"
  case res of
    288 -> putStrLn "OK"
    _   -> putStrLn "something went wrong"

{- Task 2:
To ignore the spaces between the numbers on each line of input files.
-}

parseData2 :: String -> [Int]
parseData2 x = map (read . foldl1 (++) . drop 1 . words) $ lines x

findRange :: [Int] -> Int
findRange (x:y:_) =
  let (l,u) = newRecordRange (x,y)
  in u - l + 1

task2 filename =
  findRange . parseData2 <$> readFile filename

testTask2 :: IO ()
testTask2 = do
  res <- task2 "day6-example"
  case res of
    71503 -> putStrLn "OK"
    _     -> putStrLn "something went wrong"

main :: IO ()
main = do
  x <- task1 "day6-input"
  y <- task2 "day6-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 1: " ++ show y
