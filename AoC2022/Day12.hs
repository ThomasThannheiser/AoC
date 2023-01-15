module Day12 where

import AoCHelper (Pair, between, (.+.))
import Data.List (elemIndices)
import Data.Set (difference, empty, fromList, singleton, union)

type Dimension = Pair Int

neighbors :: Dimension -> Pair Int -> [Pair Int]
neighbors (m, n) pos =
  filter (\(r, c) -> r `between` (0, m - 1) && c `between` (0, n - 1))
  . map (pos .+.) $ [(1, 0), (-1, 0), (0, 1), (0, -1)]

realNbs :: [String] -> Dimension -> Pair Int -> [Pair Int]
realNbs input dim pt = filter pred . neighbors dim $ pt
  where
    pred nb = charAt nb <= (succ . charAt $ pt)
    charAt (row, col) = case input !! row !! col of
      'S' -> 'a'
      'E' -> 'z'
      c -> c

bfs :: [String] -> Pair Int -> Pair Int -> Int
bfs input start = bfs' 0 (singleton start) empty
  where
    m = length input
    n = length $ head input
    bfs' count toVisit visited goal
      | null toVisit = -1
      | goal `elem` toVisit = count
      | otherwise = let toVisit' = fromList (concatMap (realNbs input (m, n)) toVisit) `difference` visited in 
                    bfs' (count + 1) toVisit' (visited `union` toVisit) goal

coordinates :: Eq a => a -> [[a]] -> [Pair Int]
coordinates c input = map (`quotRem` n) . elemIndices c $ concat input
  where n = length $ head input

day12 :: [String] -> (Pair Int -> Int)
day12 input = flip (bfs input) e
  where e = head $ coordinates 'E' input

day12_1, day12_2 :: [String] -> Int
day12_1 input = day12 input s
  where s = head $ coordinates 'S' input

day12_2 input = minimum . filter (> 0) . map (day12 input) $ s : as
  where s = head $ coordinates 'S' input
        as = coordinates 'a' input

-- 468
-- 459

main :: IO ()
main = do
  input <- readFile "day12.input"
  print . day12_1 . lines $ input
  print . day12_2 . lines $ input