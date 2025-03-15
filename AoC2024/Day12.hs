module Day12 where

import AoCHelper (Grid, Pair, between, both, cross, (.+.), (.-.), (@))
import Data.Function (on)
import Data.List (groupBy, sort, transpose, unfoldr, (\\), intersect)
import Data.Set as Set (difference, empty, fromList, singleton, toList, union)
import Data.Tuple (swap)

neighbors :: Pair Int -> [Pair Int]
neighbors pos = map (pos .+.) cross

realNbs :: Grid Char -> Char -> Pair Int -> [Pair Int]
realNbs grid c = filter ((== c) . (grid @)) . neighbors

bfs :: Grid Char -> Pair Int -> [Pair Int]
bfs grid start = bfs' (singleton start) empty
  where
    c = grid @ start
    bfs' toVisit visited
      | null toVisit = toList visited
      | otherwise =
          let toVisit' = fromList (concatMap (realNbs grid c) toVisit) `difference` visited
           in bfs' toVisit' (visited `union` toVisit)

calculateRegions :: Grid Char -> [Pair Int] -> [[Pair Int]]
calculateRegions grid = unfoldr f
  where
    f [] = Nothing
    f (p : ps) = let newRegion = bfs grid p
                  in Just (newRegion, ps \\ newRegion)

day12 :: Grid Char -> [[Pair Int]]
day12 input = calculateRegions grid points
  where
    points = (,) <$> [1 .. dim] <*> [1 .. dim]
    dim = length input
    grid = transpose . addDot . transpose . addDot $ input
    addDot = map (reverse . ('.' :) . reverse . ('.' :))

day12_1, day12_2 :: Grid Char -> Int
day12_1 = sum . (zipWith (*) <$> map length <*> map f) . day12
  where
    f = ((+) `on` (sum . map g . groupBy ((==) `on` fst) . sort)) <*> map swap
    g = (2 *) . succ . length . filter ((> 1) . snd) . (zipWith (.-.) =<< tail)

day12_2 = sum . (zipWith (*) <$> map length <*> map h) . day12
  where
    h reg = sum . map (g . (`intersect` reg) . f) $ frame reg
    f p = map (p .+.) [(0, 0), (0,1), (1,0), (1,1)]
    g xs 
      | length xs `elem` [1, 3] = 1
      | length xs == 2 = let [(x1, y1), (x2, y2)] = xs 
                          in if x1 == x2 || y1 == y2 then 0 else 2  
      | otherwise = 0
        
    frame reg = [(x, y) | let minX = pred . minimum $ map fst reg,
                          let maxX = maximum $ map fst reg,
                          let minY = pred . minimum $ map snd reg,
                          let maxY = maximum $ map snd reg, 
                          x <- [minX .. maxX], y <- [minY .. maxY]]

-- 1573474
--  966476

main :: IO ()
main = do
  input <- readFile "day12.input"
  print . day12_1 $ lines input
  print . day12_2 $ lines input