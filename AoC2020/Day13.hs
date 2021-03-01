module Day13 where

import AoCHelper (split')
import Data.List (elemIndex)
import Data.Bifunctor (second)

toLst :: [String] -> [String]
toLst = split' (== ',') . (!! 1)

day13_1 lst = factor * b !! idx
  where (a, b) = (read . head $ lst, map read . filter (/= "x") . toLst $ lst)
        modDiff = zipWith (-) b (map (mod a) b)
        factor = minimum modDiff
        Just idx = elemIndex factor modDiff

day13_2 lst = mod (sum $ zipWith (*) (map fst diffLst) (map calc sndLst)) m
  where diffLst = map (second read) . filter ((/= "x") . snd) $ zip [0..] (toLst lst)
        sndLst = map snd diffLst
        m = product sndLst
        -- Chinese remainder theorem!
        calc mi = (head . filter ((== 0) . (`mod` mi)) $ iterate (+ negate (div m mi)) 1) - 1

-- 2305
-- 552612234243498


main = do
    input <- readFile "day13.input"
    print . day13_1 . lines $ input
    print . day13_2 . lines $ input