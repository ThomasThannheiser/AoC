module AoC2019 where

fuel :: Int -> Int
fuel n = (n `div` 3) - 2

iterFuel :: Int -> Int
iterFuel = sum . takeWhile (> 0) . tail . iterate fuel

day1_1 = sum . map (fuel . read) 
day1_2 = sum . map (iterFuel . read)

-- 3406432
-- 5106777


main = do
  input <- readFile "1_2019.txt"
  print . day1_1 $ lines input
  print . day1_2 $ lines input