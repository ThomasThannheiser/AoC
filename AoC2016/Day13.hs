module Day13 where

import AoCHelper (Pair, both, between, cross, (.+.))
import Data.Bits (popCount)
import Data.Set (empty, fromList, difference, singleton, union)

day13_1, day13_2 :: Int
day13_1 = bfs (1, 1) (31, 39)

-- may be there is a faster solution using the visited
day13_2 = length . filter (`between` (0, 50)) $ [bfs (1, 1) pair | pair <- (,) <$> [0 .. 51] <*> [0 .. 51]]

open :: Int -> Int -> Bool
open x y = even . popCount $ x * x + 3 * x + 2 *x * y + y + y * y + input
  where input = 1362

neighbors :: Pair Int -> [Pair Int]
neighbors pos = filter ((== (True, True)) . both (>= 0)) $ map (pos .+.) cross

realNbs :: Pair Int -> [Pair Int]
realNbs = filter (uncurry open) . neighbors

bfs :: Pair Int -> Pair Int -> Int
bfs start = bfs' 0 (singleton start) empty
  where
    bfs' count toVisit visited goal
      | null toVisit = -1
      | goal `elem` toVisit = count
      | otherwise =
          let toVisit' = fromList (concatMap realNbs toVisit) `difference` visited
           in bfs' (count + 1) toVisit' (visited `union` toVisit) goal

-- 82
-- 138

main :: IO ()
main = do
    print day13_1
    print day13_2