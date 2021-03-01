module Day22 where

import AoCHelper (split')
import Data.Set (empty, insert, member, Set)

toDesk :: [String] -> ([Int], [Int])
toDesk lst = (p1, p2)
  where [p1, p2] = map (map read . tail) . split' null $ lst

shift :: Bool -> ([Int], [Int]) -> ([Int], [Int])
shift True  (x : xs, y : ys) = (xs ++ [x, y], ys)
shift False (x : xs, y : ys) = (xs, ys ++ [y, x])

play :: ([Int], [Int]) -> ([Int], [Int])
play (x : xs, y : ys) = play . shift (x > y) $ (x : xs, y : ys) 
play (x, y) = (x, y)

play' :: ([Int], [Int]) -> Set ([Int], [Int]) -> (Bool, [Int])
play' ([], ys) _ = (False, ys)
play' (xs, []) _ = (True, xs)
play' g@(x : xs, y : ys) history =
  if member g history then (True, x : xs)
                      else play' next (insert g history)
  where next = shift (if x <= length xs && y <= length ys
                      then fst (play' (take x xs, take y ys) empty)
                      else x > y) g

winnerDesk :: ([Int], [Int]) -> [Int]
winnerDesk ([], ys) = ys
winnerDesk (xs, []) = xs

day22_1 lst = sum $ zipWith (*) [50, 49..] (winnerDesk game)
  where game = play . toDesk $ lst
        
day22_2 lst = sum $ zipWith (*) [50, 49..] (snd game)
  where game = play' (toDesk lst) empty

-- 35005
-- 32751


main = do
  input <- readFile "day22.input"
  print . day22_1 . lines $ input
  print . day22_2 . lines $ input