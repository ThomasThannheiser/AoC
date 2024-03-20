module Day10 where

import AoCHelper (Grid, Pair, gridAt, chunksOf, (.+.))
import Data.List (elemIndices, unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map

data Direction = N | S | W | E

dirToInts :: Direction -> Pair Int
dirToInts N = (-1, 0)
dirToInts S = (1, 0)
dirToInts W = (0, -1)
dirToInts E = (0, 1)

findS :: Grid Char -> Pair Int
findS input = (`quotRem` n) . head . elemIndices 'S' $ concat input
  where
    n = length $ head input

symbolAtS :: Grid Char -> Char
symbolAtS input
  | upOpen && downOpen = '|'
  | upOpen && rightOpen = 'L'
  | upOpen && leftOpen = 'J'
  | downOpen && rightOpen = 'F'
  | downOpen && leftOpen = '7'
  | otherwise = '-'
  where
    (y, x) = findS input
    g (y', x') = input !! y' !! x'
    upOpen = g (y - 1, x) `elem` "F|7"
    downOpen = g (y + 1, x) `elem` "L|J"
    rightOpen = g (y, x + 1) `elem` "7-J"
    leftOpen = g (y, x - 1) `elem` "F-L"

c2d :: Char -> Direction
c2d c
  | c == '-' = E
  | c `elem` "F|7" = N
  | otherwise = S

day10 :: Grid Char -> [(Pair Int, Direction)]
day10 input = ((s, d) :) . takeWhile ((/= s) . fst) . tail $ iterate f (s, d)
  where
    s = findS input
    f = nextStep input
    d = c2d $ symbolAtS input

day10_1, day10_2 :: Grid Char -> Int
day10_1 = (`div` 2) . length . day10
day10_2 = sum . map calcInnerCount . markLoop

nextStep :: Grid Char -> (Pair Int, Direction) -> (Pair Int, Direction)
nextStep grid ((y, x), d) = ((y, x) .+. dirToInts d', d')
  where
    d' = nextDir c d
    c = case grid !! y !! x of
      'S' -> symbolAtS grid
      c -> c

nextDir :: Char -> Direction -> Direction
nextDir 'F' N = E
nextDir 'F' W = S
nextDir 'L' W = N
nextDir 'L' S = E
nextDir 'J' S = W
nextDir 'J' E = N
nextDir '7' E = S
nextDir '7' N = W
nextDir _ d = d

markLoop :: Grid Char -> [String]
markLoop input = chunksOf n [g (y, x) | y <- [0 .. pred m], x <- [0 .. pred n]]
  where
    g (y, x)
      | (y, x) == s = symbolAtS input
      | Map.member (y, x) cdMap = gridAt input (y, x)
      | otherwise = '.'
    cdMap = Map.fromList $ day10 input
    m = length input
    n = length $ head input
    s = findS input

calcInnerCount :: String -> Int
calcInnerCount = length . filter id . unfoldr f
  where
    f [] = Nothing
    f (x : xs) = Just (isInner (x : xs), xs)
    isInner ('.' : xs) = odd . length $ filter (`elem` "F|7") xs
    isInner _ = False

-- 6820
-- 337

main :: IO ()
main = do
  input <- readFile "day10.input"
  print . day10_1 . lines $ input
  print . day10_2 . lines $ input

--   writeFile "day10.output" . unlines . markLoop $ lines input
