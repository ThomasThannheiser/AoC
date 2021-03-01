module AoC2016 where

import AoCHelper (split')
import Data.List (sort, groupBy, inits, group)

day1 input = (e-w, s-n)
    where [e, s, w, n] = map (sum . map snd) . groupBy (\x y -> fst x == fst y) . sort . zip firsts $ map (read . tail) input
          firsts = map ((`mod` 4) . ($0)) . scanl1 (.) . map ((\ x -> if x == 'R' then (+ 1) else (\ y -> y - 1)) . head) $ input
          
toInput :: [Char] -> [[Char]]
toInput = map tail . split' (== ',') . (' ':)

day1_1 :: (Num a, Ord a, Read a) => [Char] -> a
day1_1 lst = abs x + abs y
    where (x, y) = day1 . toInput $ lst

-- to be continued
day1_2 = map day1 . drop 6 . inits . toInput 
    
-- 300
-- 307       

main = do
    input <- readFile "1_2016.txt"
    print . day1_1 $ input
    print . day1_2 $ input