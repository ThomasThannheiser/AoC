module AoC2017 where

import AoCHelper (split')
import Data.Char (digitToInt)
import Data.Set (size, fromList)
import Data.List (sort)

day1 input = sum . map (digitToInt . fst) . filter (uncurry (==)) . zip input
day1_1 lst = day1 (last lst : lst) lst
day1_2 lst = day1 input lst
    where input = b ++ a
          (a, b) = splitAt ( length lst `div` 2) lst

-- 1119
-- 1420

day2 :: ([[Int]] -> [Int]) -> [String] -> Int
day2 fkt = sum . fkt . map (map read . split' (== '\9'))
day2_1 = day2 $ map (\xs -> maximum xs - minimum xs)
day2_2 = day2 $ concatMap (\xs -> [x `div` y | x <- xs, y <- xs, x > y, (x `mod` y) == 0])

-- 50376
-- 267

day3_1 input = 1 + (length . takeWhile (<= read input) $ [x*x | x <- [1..]])
-- plus some thinking about it!-)

-- 552

day4 = length . filter (\xs -> size (Data.Set.fromList xs) == length xs)
day4_1 = day4 . map words 
day4_2 =  day4 . map (map sort . words)

-- 466
-- 251


main = do
  input <- readFile "3_2017.txt"
  print . day3_1  $ input
  -- print . day4_2 . lines $ input
  