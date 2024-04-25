module Main where

import Data.List.Split (splitOn)
import Data.Tuple.Extra (thd3, both)
import qualified Data.Array as A

type Block = [(Int, Int, Int)]
type Brick = (Int, [Int])

-- Parse input file --

makeBlock :: String -> Block
makeBlock = blocks . map (\x -> read $ "[" <> x <> "]") . splitOn "~"
  where
    blocks [[a, b, c], [d, e, f]] = [(x, y, z)
                                    | x <- [min a d .. max a d]
                                    , y <- [min b e .. max b e]
                                    , z <- [min c f .. max c f]]

makeBlocks :: String -> [Block]
makeBlocks = map makeBlock . lines

-- Settle all bricks --

-- Test whether the second block is supported by any block from a list of blocks
isNotSupported :: [Block] -> Block -> Bool
isNotSupported blocks block = all unsupportedPosition $ bottomPositions block
  where
    unsupportedPosition (x, y, z) = not (any ((x, y, z - 1) `elem`) blocks)
    bottomPositions = filter ((== minZ) . thd3)
    minZ = minimum $ map thd3 block

-- Move those blocks, where it is possible, one step down
fallBlocks :: [Block] -> [Block]
fallBlocks blocks = map fallBlock blocks
  where
    fallBlock block =
      if isNotSupported blocks block && all ((> 1) . thd3) block
      then map (\(x, y, z) -> (x, y, z - 1)) block
      else block

-- Move blocks iteratively down until they settle
settleBlocks :: [Block] -> [Brick]
settleBlocks xs =
  if xs == xs' then findSupport $ zip [1 ..] xs else settleBlocks xs'
  where
    xs' = fallBlocks xs

-- Test whether the first block is supported by the second block
isSupportedBy :: Block -> Block -> Bool
isSupportedBy block support =
  any (\(x, y, z) -> (x, y, z - 1) `elem` support) block

-- Find a list of supporting bricks for all bricks
findSupport :: [(Int, Block)] -> [Brick]
findSupport xs = map (support xs) xs
  where
    support bricks brick = (fst brick, concatMap (supportedBy brick) bricks)
    supportedBy (x, xs) (y, ys) = [y | x /= y && isSupportedBy xs ys] 

-- Task 1 --

-- Find bricks which can be desintegrated and count them
findDisintegratable :: [Brick] -> Int
findDisintegratable xs =
  length $ filter (1 `notElem`) $ map supported allBricks
  where
    supported x = map (length . snd) $ filter ((x `elem`) . snd) xs
    allBricks = map fst xs 

task1 filename = findDisintegratable . settleBlocks . makeBlocks
  <$> readFile filename

-- Task 2 --

-- Find bricks which are fully supported by a list of bricks
fullSupport :: [Brick] -> [Int] -> [Int]
fullSupport bricks xs =
  map fst $ filter (\(_, ys) -> all (`elem` xs) ys) bricks

-- Find which bricks fall if a given brick is disintegrated
falling :: [Brick] -> Int -> [Int]
falling bricks brick = go bricks (brick : [])
  where
    go bricks falled = if null newFalled
      then falled
      else go (reducedStack bricks) (falled <> newFalled)
      where
        reducedStack xs = filter ((`notElem` falled) . fst) xs
        newFalled = fullSupport (reducedStack bricks) falled

-- Count fallen bricks as a result of disintegrating each brick
countFalled :: [Brick] -> Int
countFalled bricks =
  sum $ map ((\x -> x - 1) . length . falling supportedBricks) brickList
  where
    brickList = map fst bricks
    supportedBricks = filter (not . null . snd) bricks

task2 filename = countFalled . settleBlocks . makeBlocks <$> readFile filename

-- Useful staff --

testTask1 = (\x -> if x == 5 then "OK" else "Something went wrong")
  <$> task1 "day22-example"

testTask2 = (\x -> if x == 7 then "OK" else "Something went wrong")
  <$> task2 "day22-example"

main = do
  x <- task1 "day22-input"
  y <- task2 "day22-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
