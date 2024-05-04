module Main where

import Data.List.Split (splitOn)
import Data.Maybe (maybeToList)
import qualified Data.Matrix as Mat

data Position = Position{xPos :: Int, yPos :: Int, zPos :: Int} deriving (Show, Eq, Ord)

data Velocity = Velocity{xVel :: Int, yVel :: Int, zVel :: Int} deriving (Show, Eq, Ord)

data HailStone = HS{ getPos :: Position, getVel :: Velocity} deriving (Show, Eq, Ord)

-- Parsing input file --

readVelocity :: String -> Velocity
readVelocity text = Velocity (xs !! 0) (xs !! 1) (xs !! 2)
  where
    xs = read $ "[" <> text <> "]"

readPosition :: String -> Position
readPosition text = Position (xs !! 0) (xs !! 1) (xs !! 2)
  where
    xs = read $ "[" <> text <> "]"

parseLine :: String -> HailStone
parseLine text = HS (readPosition pos) (readVelocity vel)
  where
    [pos, vel] = splitOn " @ " text

parseData :: String -> [HailStone]
parseData = map parseLine . lines

-- Task 1 --

-- Test whether hailstones' paths cross in one point
oneColision :: HailStone -> HailStone -> Bool
oneColision (HS (Position px1 py1 _) (Velocity vx1 vy1 _))
            (HS (Position px2 py2 _) (Velocity vx2 vy2 _))
  | (px1 - px2) * vy1 == (py2 - py1) * vx2 = False
  | vx1 * vy2 == vy1 * vx2 = False
  | otherwise = True

-- Find a position (in x-y plane, ignoring z axis) in which hailstones' paths
-- intersect in future
colideFuturePos :: HailStone -> HailStone -> Maybe (Rational, Rational)
colideFuturePos h1@(HS (Position px1 py1 _) (Velocity vx1 vy1 _))
                h2@(HS (Position px2 py2 _) (Velocity vx2 vy2 _))
  | not $ oneColision h1 h2 = Nothing
  | otherwise = if t1 > 0 && t2 > 0
                then Just (toRational px2 + t2 * toRational vx2,
                           toRational py2 + t2 * toRational vy2)
                else Nothing
  where
    t1 = toRational ((py1 - py2) * vx2 - (px1 - px2) * vy2 ) /
         toRational (vx1 * vy2 - vy1 * vx2)
    t2 = toRational ((py2 - py1) * vx1 - (px2 - px1) * vy1 ) /
         toRational (vx2 * vy1 - vy2 * vx1)

-- Count pairs of hailstones's path which cross within given area in future
countCrossings :: (Int, Int) -> [HailStone] -> Int
countCrossings (minPos, maxPos) xs =
  length . filter insideArea . concatMap (maybeToList . uncurry colideFuturePos) $ comb
  where
    comb = [ (x, y) | x <- xs, y <- xs, x < y]
    (minPos', maxPos') = (toRational minPos, toRational maxPos)
    insideArea (xPos, yPos) =
      minPos' <= xPos && maxPos' >= xPos && minPos' <= yPos && maxPos' >= yPos

task1 limitRange filename =
  (countCrossings limitRange) . parseData <$> readFile filename

-- Task 2 --

{-
Solution based on equations according to 
  https://www.reddit.com/r/adventofcode/comments/18q40he/2023_day_24_part_2_a_straightforward_nonsolver/

A system of linear equations
   A * [posX, posY, posZ, velX, velY, velZ] = B
constructed based on three hailstones, enabling to set initial position and
velocity of a rock, in order for the rock to hit all three hailstones in future.

Next, testing whether the rock will hit all available hailstones.
-}

-- Defining A matrix of linear equations (coeficients of variables).
-- To make the matrix of square shape, excessive equations were ignored
matrixA :: [HailStone] -> Mat.Matrix Rational
matrixA [(HS (Position px1 py1 pz1) (Velocity vx1 vy1 vz1)),
         (HS (Position px2 py2 pz2) (Velocity vx2 vy2 vz2)),
         (HS (Position px3 py3 pz3) (Velocity vx3 vy3 vz3))] =
  Mat.mapPos (\(_, _) x -> toRational x) $ Mat.fromLists
  [
    [vy2 - vy1, vx1 - vx2, 0,         py1 - py2, px2 - px1, 0        ],
    [vy3 - vy1, vx1 - vx3, 0,         py1 - py3, px3 - px1, 0        ],
--  [vy3 - vy2, vx2 - vx3, 0,         py2 - py3, px3 - px2, 0        ],
    [vz2 - vz1, 0,         vx1 - vx2, pz1 - pz2, 0,         px2 - px1],
--  [vz3 - vz1, 0,         vx1 - vx3, pz1 - pz3, 0,         px3 - px1],
    [vz3 - vz2, 0,         vx2 - vx3, pz2 - pz3, 0,         px3 - px2],
--  [0,         vz2 - vz1, vy1 - vy2, 0,         pz1 - pz2, py2 - py1],
    [0,         vz3 - vz1, vy1 - vy3, 0,         pz1 - pz3, py3 - py1],
    [0,         vz3 - vz2, vy2 - vy3, 0,         pz2 - pz3, py3 - py2]
  ]

-- Defining columnar matrix B of linear equations
matrixB :: [HailStone] -> Mat.Matrix Rational
matrixB [(HS (Position px1 py1 pz1) (Velocity vx1 vy1 vz1)),
         (HS (Position px2 py2 pz2) (Velocity vx2 vy2 vz2)),
         (HS (Position px3 py3 pz3) (Velocity vx3 vy3 vz3))] =
  Mat.mapPos (\(_, _) x -> toRational x) $ Mat.fromLists
  [
    [px2 * vy2 - px1 * vy1 + py1 * vx1 - py2 * vx2],
    [px3 * vy3 - px1 * vy1 + py1 * vx1 - py3 * vx3],
--  [px3 * vy3 - px2 * vy2 + py2 * vx2 - py3 * vx3],
    [px2 * vz2 - px1 * vz1 + pz1 * vx1 - pz2 * vx2],
--  [px3 * vz3 - px1 * vz1 + pz1 * vx1 - pz3 * vx3],
    [px3 * vz3 - px2 * vz2 + pz2 * vx2 - pz3 * vx3],
--  [py2 * vz2 - py1 * vz1 + pz1 * vy1 - pz2 * vy2],
    [py3 * vz3 - py1 * vz1 + pz1 * vy1 - pz3 * vy3],
    [py3 * vz3 - py2 * vz2 + pz2 * vy2 - pz3 * vy3]
  ]

-- Set the start position and velocity of a rock to hit hailstones
-- by solving linear equations 
startPos :: [HailStone] -> HailStone
startPos hails = case mA' of
  Right x -> makePos $ map truncate $ Mat.toList $ Mat.multStd x mb
  Left e  -> error e
  where
    mA' = Mat.inverse $ matrixA $ take 3 hails
    mb = matrixB $ take 3 hails
    makePos [a,b,c,d,e,f] = HS (Position a b c) (Velocity d e f)

-- Test, vheteher two hailstones (the rock and a hailstone) colide
-- in future (considering all axes)
colide :: HailStone -> HailStone -> Bool
colide (HS (Position px1 py1 pz1) (Velocity vx1 vy1 vz1))
       (HS (Position px2 py2 pz2) (Velocity vx2 vy2 vz2))
  | vx1 == vx2 && px1 /= px2 = False
  | vy1 == vy2 && py1 /= py2 = False
  | vz1 == vz2 && pz1 /= pz2 = False
  | vx1 /= vx2 && vy1 /= vy2 && t px1 px2 vx1 vx2 /= t py1 py2 vy1 vy2 = False
  | vx1 /= vx2 && vz1 /= vz2 && t px1 px2 vx1 vx2 /= t pz1 pz2 vz1 vz2 = False
  | vy1 /= vy2 && vz1 /= vz2 && t py1 py2 vy1 vy2 /= t pz1 pz2 vz1 vz2 = False
  | vx1 /= vx2 && t px1 px2 vx1 vx2 < 0 = False
  | vy1 /= vy2 && t py1 py2 vy1 vy2 < 0 = False
  | vz1 /= vz2 && t pz1 pz2 vz1 vz2 < 0 = False
  | otherwise = True
  where
    t p1 p2 v1 v2 = toRational (p2 - p1) / toRational (v1 - v2)

-- Find start position and velocity of the rock, test whether it will hit
-- all hailstones in future, and calculate checksum of the rock's position
findStart :: [HailStone] -> Int
findStart hails = if all id $ map (colide start) hails
  then sum [xPos posStart, yPos posStart, zPos posStart]
  else error "Not able to hit all hailstorms"
  where
    start = startPos hails
    posStart = getPos start

task2 filename = findStart . parseData <$> readFile filename

-- Useful staff --

testTask1 = (\x -> if x == 2 then "OK" else "Something went wrong")
  <$> task1 (7, 27) "day24-example"

testTask2 = (\x -> if x == 47 then "OK" else "Something went wrong")
  <$> task2 "day24-example"

main = do
  x <- task1 (200000000000000, 400000000000000) "day24-input"
  y <- task2 "day24-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
