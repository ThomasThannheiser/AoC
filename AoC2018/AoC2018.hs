{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use notElem" #-}
module AoC2018 where

import MPCAS (Parser, anyChar, char, symbol, runParser, integer, parenthezised)
import AoCHelper ((.+.))
import Data.Char (ord, toUpper)
import Data.List (group, sort, (\\), sortBy)
import Data.Set (elemAt, fromList, difference, toList)
import Data.Text.Internal.Read (IParser(runP))

{-- day 1 --}

day1 :: [String] -> [Integer]
day1 = map (read . filter (/= '+'))

day1_1 :: [String] -> Integer
day1_1 = sum . day1

-- day1_2 lst = findFirstDublicateIn $ scanl (+) 0 $ cycle . day1 $ lst
day1_2 input = map (\x -> (x `mod` 522, x `div` 522)) lst
  where
    lst = scanl (+) 0 . day1 $ input

-- 522

{-- day 2 --}

day2_1 :: Ord a => [[a]] -> Int
day2_1 input = f 3 * f 2
  where
    f n = length . filter (n `elem`) $ counts
    counts = map (map length . group . sort) input

-- 4920

{-- day 3 --}

-- implemented in Elm

-- 116140
-- 574

{-- day 5 --}

collapse :: String -> String
collapse (x : y : rest) = if abs (ord x - ord y) == 32 then collapse rest else x : collapse (y : rest)
collapse xs = xs

day5 :: String -> String
day5 input = xs !! n
  where
    n = length . takeWhile id . zipWith (>) l $ tail l
    l = map length xs
    xs = iterate collapse input

cut :: Char -> String -> String
cut c = filter (\x -> x /= c && x /= toUpper c)

day5_1, day5_2 :: String -> Int
day5_1 = length . day5
day5_2 xs = minimum . map (($ xs) . (\c -> day5_1 . cut c)) $ ['a' .. 'z']

-- 10972
-- 5278

{-- day 7 --}

extract :: Parser Maybe (Char, Char)
extract = (,) <$> (symbol "Step" *> anyChar) <*>
                  (symbol "must be finished before step" *> anyChar <* symbol "can begin.")

parse :: String -> (Char, Char)
parse = maybe ('.', '.') fst . runParser extract


fkt :: (String, [(Char, Char)]) -> (String, [(Char, Char)])
fkt (c, []) = (reverse c ++ sort (['A'..'Z'] \\ c), [])
fkt (c, lst) = (r : c, rest)
    where
        rest = filter (\(x, y) -> x /= r) lst
        r = minimum . toList $ ((fromList . map fst $ lst) `difference` (fromList . map snd $ lst))

day7_1 :: [String] -> String
day7_1 input =
    (map fst . iterate fkt $ ("", map parse input)) !! 26

-- FDSEGJLPKNRYOAMQIUHTCVWZXB

{-- day 11 --}

powerLevel :: (Int, Int) -> Int
powerLevel (x, y) = ((((rackID * y + serialNo) * rackID) `mod` 1000) `div` 100) - 5
  where
    rackID = x + 10
    serialNo = 9810

sum3x3 :: (Int, Int) -> Int
sum3x3 (x, y) = sum [powerLevel (x + dx, y + dy) | dx <- [0..2], dy <- [0..2]] 

day11_1 :: (Int, Int)
day11_1 = minimum . map fst . filter (\ s -> snd s == maxSum) $ sums
  where 
    maxSum = maximum . map snd $ sums
    sums = [((x, y), sum3x3 (x, y)) | x <- [1..298], y <- [1..298]]

-- 245,14

{-- day 23 --}

type Point = (Int, Int, Int)

extractNanobot :: Parser Maybe (Point, Int)
extractNanobot = do
  symbol "pos=<"
  x <- integer
  char ','
  y <- integer
  char ','
  z <- integer
  symbol ">, r="
  r <- integer
  return ((x, y, z), r)

parseNanobot :: String -> (Point, Int)
parseNanobot = maybe ((0,0,0), 0) fst . runParser extractNanobot

day23_1 input = length . filter (\(p, r) -> distance p (fst greatest) <= snd greatest) $ sorted
  where
    greatest = last sorted
    sorted = sortBy (\x y -> compare (snd x) (snd y)) . map parseNanobot $ input
    distance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

-- 172

main = do
  input <- readFile "23_2018.txt"
  print . day23_1 . lines $ input
