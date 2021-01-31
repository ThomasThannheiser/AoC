module Day15 where

import AoCHelper (iter, readIntLst)
import Data.List (elemIndex)
import Data.IntMap (fromList, lookup, insert, IntMap)

nextElem :: [Int] -> [Int]
nextElem input@(x : xs) = case elemIndex x xs of
  Nothing -> 0: input
  Just n -> succ n : input
  
nextElem' :: (IntMap Int, (Int, Int)) -> (IntMap Int, (Int, Int))
nextElem' (input, (turn, last)) =
  (insert last turn input,
    (succ turn,
        case Data.IntMap.lookup last input of
            Nothing -> 0
            Just x -> turn - x))

day15_1 = head . iter nextElem (2020-7) . reverse . readIntLst
day15_2 lst = snd . iter nextElem' (30000000-8) $ (input, (8,0)) -- Part 2 take 10 minutes !-(
  where input = fromList $ zip (readIntLst lst) [1..]

-- 468
-- 1801753


main = do
  input <- readFile "day15.input"
  print . day15_1 $ input
  print . day15_2 $ input