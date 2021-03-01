module Day23 where

import AoCHelper (iter)
import Data.Char (digitToInt)
import Data.IntMap (lookup, insert, fromList, IntMap)

maxVal = 9

getInputValue :: Int -> [Int] -> Int
getInputValue start = check (start-1) 
    where check 0 lst = check maxVal lst 
          check n [n1, n2, n3] = if n == n1 || n == n2 || n == n3 
                                 then check (n-1) [n1, n2, n3] else n 

step :: (IntMap Int, Int) -> (IntMap Int, Int)
step (dict, start) = (insert m n1 (insert n3 mNext (insert start ns dict)), ns)   
    where Just n1 = Data.IntMap.lookup start dict
          Just n2 = Data.IntMap.lookup n1 dict
          Just n3 = Data.IntMap.lookup n2 dict
          Just ns = Data.IntMap.lookup n3 dict
          m = getInputValue start [n1, n2, n3]
          Just mNext = Data.IntMap.lookup m dict

getResult :: IntMap Int -> Int
getResult dict = read . concatMap show . tail . reverse . pick $ [1]
    where pick lst@(x : xs) = let Just y = Data.IntMap.lookup x dict 
                              in if y `elem` lst then lst else pick(y : lst)
          
day23_1 input = getResult . fst $ iterate step (fromList dict, start) !! 100
    where start = snd . head $ dict
          dict = zip lst (tail lst)
          lst = map digitToInt (last input : input)
          
-- not the right data structure? stackoverflow under ghci
day23_2 input = x * y
    where Just x = Data.IntMap.lookup 1 it
          Just y = Data.IntMap.lookup x it 
          it = fst $ iterate step (fromList dict, start) !! 10000000
          start = snd . head $ dict
          dict = (maxVal, 5) : zip lst (tail lst)
          lst = map digitToInt input ++ [10..maxVal]        

main = do 
    input <- readFile "day23.input"
    print . day23_1 $ input
--    print . day23_2 $ input