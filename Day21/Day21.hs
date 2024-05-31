module Main where

import qualified Data.Array as A
import qualified Data.Set as S

type Position = (Int, Int)
type Grid = A.Array (Int, Int) Char

parseData :: String -> Grid
parseData xs = A.listArray ((1, 1),(length rows, length $ head rows)) tiles
  where
    rows = lines xs
    tiles = concat rows

-- Task 1 --

findStart :: Grid -> Position
findStart = fst . head . filter ((=='S') . snd) . A.assocs

performStep :: Grid -> (Int, Int) -> S.Set Position -> S.Set Position
performStep g (rMax, cMax) px =
  S.fromList $ concatMap (filter (\x -> '#' /= (g A.! x)) . tiles) $ S.toList px
  where
    possiblePos (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
    validPos (r, c) = r >= 1 && r <= rMax && c >= 1 && c <= cMax
    tiles pos = filter validPos $ possiblePos pos

performSteps :: Int -> Grid -> Int
performSteps n grid =
  S.size $ foldl (\r _ -> performStep grid bounds r) startPos [1 .. n]
  where
    bounds = snd $ A.bounds grid
    startPos = S.singleton $ findStart grid

-- Task 2 --

{-
  For explanation see
    https://work.njae.me.uk/2023/12/29/advent-of-code-2023-day-21/

  Ordering of blocks (repeated maps):
               7 11  9
            7  3  2  5  9
         7  3  2  1  2  5  9
      7  3  2  1  2  1  2  5  9
     12  2  1  2  1  2  1  2 13
      8  4  2  1  2  1  2  6 10
         8  4  2  1  2  6 10
            8  4  2  6 10
               8 14 10
-}

sim :: (Int, Int) -> Int -> Grid -> Int
sim start steps grid =
  S.size $ foldl (\r _ -> performStep grid bounds r) startPos [1 .. steps]
  where
    bounds = snd $ A.bounds grid
    startPos = S.singleton start

accessibleTiles :: Grid -> Int -> Int
accessibleTiles grid blocktype
  | blocktype ==  1 = sim (md, md) (md - 1 + mx + mx) grid
  | blocktype ==  2 = sim (md, md) (md - 1 + mx) grid
  | blocktype ==  3 = sim (mx, mx) (md - 1 + mx - 1) grid
  | blocktype ==  4 = sim (1,  mx) (md - 1 + mx - 1) grid
  | blocktype ==  5 = sim (mx, 1)  (md - 1 + mx - 1) grid
  | blocktype ==  6 = sim (1,  1)  (md - 1 + mx - 1) grid
  | blocktype ==  7 = sim (mx, mx) (md - 2) grid
  | blocktype ==  8 = sim (1,  mx) (md - 2) grid
  | blocktype ==  9 = sim (mx, 1)  (md - 2) grid
  | blocktype == 10 = sim (1,  1)  (md - 2) grid
  | blocktype == 11 = sim (mx, md) (mx - 1) grid
  | blocktype == 12 = sim (md, mx) (mx - 1) grid
  | blocktype == 13 = sim (md, 1)  (mx - 1) grid
  | blocktype == 14 = sim (1,  md) (mx - 1) grid
  where
    mx = fst . snd $ A.bounds grid
    md = 1 + div (mx -1) 2

calculate :: Grid -> Int
calculate grid = sum $ zipWith (*) posInBlocks numberBlocks
  where
    posInBlocks = map (accessibleTiles grid) [1 .. 14]
    numberBlocks = [(k-1)*(k-1), k*k,
                   k-1, k-1, k-1, k-1, k, k, k, k, 1, 1, 1, 1]
    k = div (26501365 - 65) 131

-- Useful staff --

testTask1 =
  (\x -> if x == 16 then "OK" else "Something went wrong")
  . performSteps 6
  . parseData
    <$> readFile "day21-example"

main :: IO ()
main = do
  x <- performSteps 64 . parseData <$> readFile "day21-input"
  y <- calculate . parseData <$> readFile "day21-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
