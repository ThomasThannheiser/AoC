module AoC2018 where

dropPlus (x : xs) = if x == '+' then xs else x : xs

day1 = map (read . dropPlus)
day1_1 = sum . day1
-- day1_2 lst = take 200 . scanl (+) 0 $ cycle . day1 $ lst

-- 522

main = do
    input <- readFile "1_2018.txt"
    print . day1_1 . lines $ input
    -- print . day1_2 . lines $ input