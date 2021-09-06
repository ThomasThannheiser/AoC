module AoC2018 where

day1 = map (read . filter (/= '+'))
day1_1 = sum . day1
-- day1_2 lst = findFirstDublicateIn $ scanl (+) 0 $ cycle . day1 $ lst
day1_2 input = map (\x -> (x `mod` 522, x `div` 522)) lst
    where lst = scanl (+) 0 . day1 $ input

-- 522

main = do
    input <- readFile "1_2018.txt"
    print . day1_1 . lines $ input
    print . day1_2 . lines $ input