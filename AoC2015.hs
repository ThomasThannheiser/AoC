module AoC2015 where

import AoCHelper (split', (.+.))
-- import MPCAS (Parser, runParser, parenthezised)
import Data.List (sort)
import Data.Set (fromList, toList, size, union)
-- import Control.Applicative (Alternative((<|>)))

day1 = map toInt
  where toInt :: Char -> Int 
        toInt '(' = 1
        toInt ')' = -1

{-- try part 2 with parsing
nested :: Parser Maybe Int 
nested = (+) <$> parenthezised nested <*> nested <|> pure 2
day1_2 input = (-1) + (maybe 0 fst . runParser nested $ input)
 --}

day1_1 = sum . day1
day1_2 = length . takeWhile (>= 0) . scanl (+) 0 . day1
day1_3 = maximum . scanl (+) 0 . day1
-- 138
-- 1771
-- 160

day2 toInt = sum . map (toInt . map read . split' (== 'x')) 
day2_1 = day2 toInt
  where toInt [a,b,c] = let [x, y, z] = [a *b, a * c, b * c] in
                        2 * (x + y + z) + minimum [x, y, z]
day2_2 = day2 toInt
  where toInt [a,b,c] = let [x, y, z] = sort [a, b, c] in
                        x * y * z + 2 * (x + y) 

-- 1606483
-- 3842356

toDir :: Char -> (Int, Int)
toDir '<' = (-1,  0)
toDir '>' = ( 1,  0)
toDir '^' = ( 0,  1)
toDir 'v' = ( 0, -1)

evenOdds :: [a] -> ([a], [a])
evenOdds (x : y : xs) = let (as, bs) = evenOdds xs in
                        (x : as, y : bs)
evenOdds xs = (xs, [])

day3 = fromList . scanl (.+.) (0,0) . map toDir
day3_1 = size . day3
day3_2 input = size $ day3 s `union` day3 r 
  where (s, r) = evenOdds input
    
-- 2081
-- 2341


main = do
    input <- readFile "1_2015.txt"
    print . day1_1 $ input
    print . day1_2 $ input
    print . day1_3 $ input