module AoCHelper where

import Data.List (unfoldr)

between :: Int -> (Int, Int) -> Bool
between value (min, max) = min <= value && value <= max

split' :: (a -> Bool) -> [a] -> [[a]]
split' p = unfoldr (f p)
  where f _ [] = Nothing
        f p (x : xs) = Just $ break p (if p x then xs else x : xs)

bin2Int :: [Bool] -> Int
bin2Int = foldr (\x y -> fromEnum x + 2 * y) 0 . reverse

iter :: (a -> a) -> Int -> (a -> a)
iter f 0 = id
iter f n = iter f (n - 1) . f 

readIntLst :: String -> [Int]
readIntLst s = read $ '[' : s ++ "]"

diff :: [Int] -> [Int]
diff lst = zipWith (-) (tail lst) lst
