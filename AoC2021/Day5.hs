module Day5 where

import MPCAS (Parser, runParser, char, natural, symbol)
import Data.List (group, sort)

type Point = (Int, Int)
type Line = (Point, Point)

extract :: Parser Maybe Line
extract = do
  x1 <- natural
  char ','
  y1 <- natural 
  symbol "->"
  x2 <- natural
  char ','
  y2 <- natural 
  return ((x1, y1), (x2, y2))

parse :: String -> Line
parse = maybe ((0, 0), (0, 0)) fst . runParser extract

isHorizontal, isVertical, isDiagonal :: Line -> Bool
isHorizontal ((x1, y1), (x2, y2)) = x1 == x2
isVertical ((x1, y1), (x2, y2)) = y1 == y2
isDiagonal ((x1, y1), (x2, y2)) = abs (x2 - x1) == abs (y2 - y1)

line2Points :: Line -> [Point]
line2Points l@((x1, y1), (x2, y2)) 
    | isHorizontal l = map (x1,) [min y1 y2 .. max y1 y2]
    | isVertical l = map (, y1) [min x1 x2 .. max x1 x2]
    | x2 - x1 == y2 - y1 = zip [min x1 x2 .. max x1 x2] [min y1 y2 .. max y1 y2]
    | otherwise = zip [min x1 x2 .. max x1 x2] (reverse [min y1 y2 .. max y1 y2])

filterLine1, filterLine2 :: Line -> Bool
filterLine1 line = isHorizontal line || isVertical line  
filterLine2 line = filterLine1 line || isDiagonal line

day5 :: (Line -> Bool) -> ([String] -> Int)
day5 filterS = length . filter (\xs -> length xs > 1) . 
  group . sort . concatMap line2Points . filter filterS . map parse

day5_1, day5_2 :: [String] -> Int
day5_1 = day5 filterLine1
day5_2 = day5 filterLine2

-- 5147
-- 16925


main = do
  print . day5_1 . lines =<< readFile "day5.input"
  print . day5_2 . lines =<< readFile "day5.input"