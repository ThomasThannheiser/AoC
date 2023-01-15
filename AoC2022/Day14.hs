{-# LANGUAGE TupleSections #-}

module Day14 where

import AoCHelper (Pair)

import MPCAS (Parser, runParser, char, intP, sepBy, symbol)
import Data.IntSet (IntSet, fromList, notMember, insert)

lineP :: Parser Maybe [Pair Int]
lineP = sepBy ((,) <$> intP <* char ',' <*> intP) (symbol "->")

parseDay14 :: String -> [Pair Int]
parseDay14 = maybe [] fst . runParser lineP

fall :: Int -> Pair Int -> IntSet -> Pair Int
fall max (x, y) blocked 
  | y == max = (x, y)
  | f (x    , y + 1) `notMember` blocked = fall' (x    , y + 1)
  | f (x - 1, y + 1) `notMember` blocked = fall' (x - 1, y + 1)
  | f (x + 1, y + 1) `notMember` blocked = fall' (x + 1, y + 1)
  | otherwise = (x, y)
  where fall' p = fall max p blocked 

f :: Num a => (a, a) -> a
f (x, y) = 1000 * x + y

day14 :: (Int -> (Int, IntSet) -> Int) -> [String] -> Int
day14 g input = g 0 (m, fromList . map (\(x, y) -> 1000 * x + y) $ blocked) 
  where 
    pts = map parseDay14 input
    blocked = concat . concatMap (\x -> zipWith f x $ tail x) $ pts
    f (x1, y1) (x2, y2) 
      | x1 == x2 = map (x1 ,) [min y1 y2..max y1 y2]
      | y1 == y2 = map (, y1) [min x1 x2..max x1 x2]
      | otherwise = []
    m = maximum . map snd $ blocked

g :: Int -> (Int, IntSet) -> Int
g n (max, blocked) = let (x, y) = fall max (500, 0) blocked in
  if y == max then n else g (n + 1) (max, insert (f (x, y)) blocked)
    
g' :: Int -> (Int, IntSet) -> Int
g' n (max, blocked) = let (x, y) = fall (max + 1) (500, 0) blocked in  
  if y == 0 then n + 1 else g' (n + 1) (max, insert (f (x, y)) blocked)
     
day14_1, day14_2 :: [String] -> Int
day14_1 = day14 g
day14_2 = day14 g'
    
-- 618
-- 26358


main :: IO ()
main = do
   input <- readFile "day14.input"
   print . day14_1 . lines $ input
   print . day14_2 . lines $ input