module Day10 where

import AoCHelper (split')
import Data.List (sort)

-- ToDo: recursion for the factors
factors = [1, 1, 2, 4, 7{--, 12, 22--}]

diff lst = zipWith (-) (tail sorted) sorted
  where sorted = sort . map read $ lst

day10_1 lst = count 1 * count 3
  where count n = (1+) . length . filter (== n) . diff $ lst
day10_2 = product . map ((factors !!) . length) . split' (== 3) . (1 :) . diff

-- 1755
-- 4049565169664


main = do
  input <- readFile "day10.input"
  print . day10_1 . lines $ input
  print . day10_2 . lines $ input