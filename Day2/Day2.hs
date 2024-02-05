module Main where

{- Task 1:
Determine which games would have been possible if the bag had been loaded
with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum
of the IDs of those games?
-}

type GameId = Int
type Trial = [(String,Int)]
type Game = (GameId,[Trial])

-- Parse one game
parseGame :: String -> Game
parseGame text =
  let (x,y) = span (/= ':') text
      [_,game] = words x
  in (read game, parseTrials y)
  where
    parseTrials [] = []
    parseTrials text =
      let (x,y) = span (/= ';') $ tail text
      in parseTrial x : parseTrials y

-- Parse one trial of a game
parseTrial :: String -> Trial
parseTrial text =
  let xs = words $ filter (/= ',') text
  in getCubes xs
  where
    getCubes [] = []
    getCubes (x1:x2:xs) = (x2, read x1) : getCubes xs

-- Number of differently coloured cubes in the bag
capacity :: String -> Int
capacity "red"   = 12
capacity "blue"  = 14
capacity "green" = 13

-- Select IDs of games which are possibile in the context of the bag content
possibleGames :: [Game] -> [GameId]
possibleGames games =
  let checked = map (\(x,y) -> (x, checkGame y)) games
  in map fst $ filter snd checked
  where
    checkGame trials =
      and [ capacity colour >= n  | trial <- trials, (colour,n) <- trial ]

task1 :: String -> IO Int
task1 filename =
  do
    games <- readFile filename
    return . sum . possibleGames . (map parseGame) . lines $ games

testTask1 :: IO ()
testTask1 =
  do
    res <- task1 "day2-example"
    case res of
      8 -> putStrLn "OK"
      _ -> putStrLn "something went wrong"

{- Task 2:
What is the fewest number of cubes of each color that could have been in the
bag to make the game possible? For each game, find the minimum set of cubes
that must have been present. What is the sum of the power of these sets?
-}

-- Product of required minimal numbers of different cubes in the bag for a game
powerOfCubes :: Game -> Int
powerOfCubes game =
  let red = limit "red"
      blue = limit "blue"
      green = limit "green"
  in red * blue * green
  where
    limit col =
      maximum [ n | trial <- snd game, (colour,n) <- trial, colour == col]

task2 :: String -> IO Int
task2 filename =
  do
    games <- readFile filename
    return . sum . (map powerOfCubes) . (map parseGame) . lines $ games

testTask2 :: IO ()
testTask2 =
  do
    res <- task2 "day2-example"
    case res of
      2286 -> putStrLn "OK"
      _ -> putStrLn "something went wrong"

main :: IO ()
main =
  do
    x <- task1 "day2-input"
    y <- task2 "day2-input"
    putStrLn $ "Task 1: " ++ show x
    putStrLn $ "Task 2: " ++ show y
