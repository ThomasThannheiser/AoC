module Day6 where

import AoCHelper (Pair, between, both, (@))
import Data.Set (Set, fromList, size)
import Data.Maybe (fromJust, isJust)

day6 :: [String] -> ([String], Set (Pair Int))
day6 grid = (grid, path)
  where
    l = length grid
    path = fromList . map (fst . fromJust) . takeWhile isJust . iterate (step grid l) $ Just (guard, N)
    guard = head [(x, y) | (x, y) <- (,) <$> [0 .. l - 1] <*> [0 .. l - 1], grid @ (x, y) == '^']

day6_1 :: [String] -> Int
day6_1 = size . snd . day6
  
step :: [String] -> Int -> Maybe (Pair Int, Direction) -> Maybe (Pair Int, Direction)
step grid l (Just (pt, dir)) = 
  if inRange next 
    then if grid @ next == '#' 
            then Just (pt, nextDir dir) 
            else Just (next, dir)
    else Nothing
  where
    next = nextP pt dir
    inRange = (== (True, True)) . both (`between` (0, l - 1))

data Direction = N | E | S | W

nextDir :: Direction -> Direction
nextDir d = case d of
  N -> E
  E -> S
  S -> W
  W -> N

nextP :: Pair Int -> Direction -> Pair Int
nextP (x, y) d = case d of
    N -> (x - 1, y)
    E -> (x, y + 1)
    S -> (x + 1, y)
    W -> (x, y - 1)

{-- alternative solution, with a list of all obstructions, but not so fast! -}

day6_1' :: [String] -> Int
day6_1' grid = size path
  where
    l = length grid
    obstructions = findChar '#'
    guard = head $ findChar '^'
    inRange = (== (True, True)) . both (`between` (0, l - 1))
    path = fromList . takeWhile inRange . map fst $ iterate (step' obstructions) (guard, N)
    findChar c = [(x, y) | (x, y) <- (,) <$> [0 .. l - 1] <*> [0 .. l - 1], grid @ (x, y) == c]

step' :: [Pair Int] -> (Pair Int, Direction) -> (Pair Int, Direction)
step' obstr (pt, dir) = if next `elem` obstr then (pt, nextDir dir) else (next, dir)
  where next = nextP pt dir
--}

main :: IO ()
main = do
  input <- readFile "day6.input"
  print . day6_1 . lines $ input
--  print . day6_2 . lines $ input
