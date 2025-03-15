module Day18 where

import AoCHelper (Pair, between, findChar)
import Data.List (intersect, union, (\\)) 
import Data.Set (fromList, toList)

{-- programming Game of Life --}

-- very slow for a grid with 100 x 100 cells!

isAlive :: [Pair Int] -> Pair Int -> Bool
isAlive = flip elem

isEmpty :: [Pair Int] -> Pair Int -> Bool
isEmpty = flip notElem

neighbs :: Pair Int -> [Pair Int]
neighbs (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1]
                                  , (x + dx) `between` (0, 99)
                                  , dy <- [-1 .. 1]
                                  , (y + dy) `between` (0, 99)] \\ [(x, y)]

liveneighbs :: [Pair Int] -> Pair Int -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: [Pair Int] -> [Pair Int]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]] `union` allTime

births :: [Pair Int] -> [Pair Int]
births b = [p | p <- toList $ fromList (concatMap neighbs b),
                isEmpty b p,
                liveneighbs b p == 3]

allTime :: [Pair Int]
allTime = [(0,0), (0,99), (99,0), (99,99)]

step :: [Pair Int] -> [Pair Int]
step b = survivors b ++ births b

day18 :: [String] -> [Pair Int]
day18 = flip findChar '#'

day18_1, day18_2 :: [String] -> Int
day18_1 = (!! 100) . map length . iterate step . day18
day18_2 = (!! 100) . map length . iterate step . (++ allTime) . day18

-- 1061
-- 1006

main = do
    input <- readFile "18_2015.txt"
    print . day18_1 . lines $ input
    print . day18_2 . lines $ input