module Day23 where

import AoCHelper (Grid, Pair, gridAt, nbh)
import Data.List ((\\))

type Path = [Pair Int]

{- day23_1 :: Grid Char -> Int
day23_1 input = pred . maximum . map length . (!!3000) $ iterate (go grid) [[(1, 1)]]
  where 
    grid = bound : input ++ [bound]
    bound = replicate 141 '#'
    l = length input

-- tooooooo slow!! -> 30 sec.

go :: Grid Char -> [Path] -> [Path]
go grid = concatMap (go' grid)
  where 
    go' grid path = g (next grid (head path)) path
    g zs path = filter (not . null) $ map (`add` path) zs
-}

day23_1 input = map length . (!! 95) $ iterate (go grid) [[(1, 1)]]
  where 
    grid = bound : input ++ [bound]
    bound = replicate 141 '#'
    l = length input

go :: Grid Char -> [Path] -> [Path]
go grid = concatMap (go' grid)
  where 
    go' grid path = g (next grid (take 2 path)) path
    g [] path = [path]
    g zs path = filter (not . null) $ map (`add` path) zs

add :: Pair Int -> Path -> Path
add z path
  | (23, 21 {- 141, 139 -}) `elem` path = path
  | z `elem` path = []
  | otherwise = z : path

next :: Grid Char -> [Pair Int] -> [Pair Int]
next grid ((y, x) : z)
  | gridAt grid (y, x) == '<' = [(y, x - 1)]
  | gridAt grid (y, x) == '>' = [(y, x + 1)]
  | gridAt grid (y, x) == 'v' = [(y + 1, x)]
  | gridAt grid (y, x) == '^' = [(y - 1, x)]
  | otherwise = (filter ((/= '#') . gridAt grid) . nbh) (y, x) \\ z

next' :: Grid Char -> [Pair Int] -> [Pair Int]
next' grid ((y, x) : z) = filter ((/= '#') . gridAt grid) $ nbh (y, x) \\ z

-- 2114

main :: IO ()
main = do
  input <- readFile "day23exp.input"
  print . day23_1 . lines $ input
-- print . day23_2 . lines $ input
