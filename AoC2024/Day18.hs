module Day18 where

import AoCHelper (Pair, between, both, cross, splitWith, (.+.))
import Data.Set (empty, difference, fromList, singleton, union)

dim :: Int
dim = 70

day18 :: Int -> [String] -> Int
day18 n input = bfs bytes (0, 0) (dim, dim)
  where
    bytes = map (both read . splitWith ',') $ take n input

day18_1 :: [String] -> Int
day18_1 = day18 1024

day18_2 :: [String] -> String
day18_2 input = input !! pred (bisection (1024, length input))
  where
    bisection (l, r)
      | r == l + 1 = r
      | r == l + 2 = if halfHasNoPath then half else r
      | otherwise = bisection $ if halfHasNoPath then (l, half) else (half, r)
      where
        half = (l + r) `div` 2
        halfHasNoPath = day18 half input == -1

neighbors :: Pair Int -> [Pair Int]
neighbors pos = filter ((== (True, True)) . both (`between` (0, dim))) $ map (pos .+.) cross

realNbs :: [Pair Int] -> Pair Int -> [Pair Int]
realNbs input = filter (`notElem` input) . neighbors

bfs :: [Pair Int] -> Pair Int -> Pair Int -> Int
bfs input start = bfs' 0 (singleton start) Data.Set.empty
  where
    bfs' count toVisit visited goal
      | null toVisit = -1
      | goal `elem` toVisit = count
      | otherwise =
          let toVisit' = fromList (concatMap (realNbs input) toVisit) `difference` visited
           in bfs' (count + 1) toVisit' (visited `union` toVisit) goal

-- 408
-- 45,16

main :: IO ()
main = do
  input <- readFile "day18.input"
  print . day18_1 $ lines input
  print . day18_2 $ lines input