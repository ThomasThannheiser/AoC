module AoC2017 where

import AoCHelper (split')
import Control.Applicative (Alternative (many, (<|>)))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set, fromList, size, toList)
import MPCAS (Parser, anyChar, char, integer, runParser, sepBy, symbol, taged)

{-- day 1 --}

day1 :: String -> String -> Int
day1 input = sum . map (digitToInt . fst) . filter (uncurry (==)) . zip input

day1_1, day1_2 :: String -> Int
day1_1 lst = day1 (last lst : lst) lst
day1_2 lst = day1 input lst
  where
    input = b ++ a
    (a, b) = splitAt (length lst `div` 2) lst

-- 1119
-- 1420

{-- day 2 --}

day2 :: ([[Int]] -> [Int]) -> [String] -> Int
day2 fkt = sum . fkt . map (map read . split' (== '\9'))

day2_1, day2_2 :: [String] -> Int
day2_1 = day2 $ map (\xs -> maximum xs - minimum xs)
day2_2 = day2 $ concatMap (\xs -> [x `div` y | x <- xs, y <- xs, x > y, (x `mod` y) == 0])

-- 50376
-- 267

{-- day 3 --}

day3_1 :: String -> Int
day3_1 input = 1 + (length . takeWhile (<= read input) $ [x * x | x <- [1 ..]])

-- plus some thinking about it!-)

-- 552
-- 330785 calculated by hand!

{-- day 4 --}

day4 :: [[String]] -> Int
day4 = length . filter (\xs -> size (fromList xs) == length xs)

day4_1, day4_2 :: [String] -> Int
day4_1 = day4 . map words
day4_2 = day4 . map (map sort . words)

-- 466
-- 251

{-- day 9 --}

removeBanged :: String -> String
removeBanged ('!' : c : r) = removeBanged r
removeBanged (x : xs) = x : removeBanged xs
removeBanged [] = []

removeGarbage :: String -> String
removeGarbage ('<' : r) = ('<' :) . removeGarbage . dropWhile (/= '>') $ r
removeGarbage (x : r) = x : removeGarbage r
removeGarbage [] = []

sumUp :: String -> Int
sumUp s = sumUp' s 0 0
  where
    sumUp' ('{' : r) acc depth = sumUp' r acc $ succ depth
    sumUp' ('}' : r) acc depth = sumUp' r (acc + depth) $ pred depth
    sumUp' ( _  : r) acc depth = sumUp' r acc depth
    sumUp' [] acc depth = acc

day9_1, day9_2 :: String -> Int
day9_1 = sumUp . removeGarbage . removeBanged
day9_2 s = length withoutBanged - length (removeGarbage withoutBanged)
  where
    withoutBanged = removeBanged s

-- 14212
-- 6569

{-- day 11 --}

symbols = ["n", "s", "se", "sw", "ne", "nw"]

direction = [(2, 0), (-2, 0), (-1, 2), (-1, -2), (1, 2), (1, -2)]

symDict = zip symbols direction

add (x, y) = bimap (+ x) (+ y)

steps :: (Int, Int) -> Int
steps (x, y) = (x' - ry) `div` 2 + ry
  where
    x' = abs x
    y' = abs y
    ry = y' `div` 2

day11 :: [Char] -> [(Int, Int)]
day11 = fromMaybe [] . mapM (`lookup` symDict) . splitOn ","

day11_1, day11_2 :: String -> Int
day11_1 = steps . foldr add (0, 0) . day11
day11_2 = maximum . map steps . scanl add (0, 0) . day11

-- 810
-- 1567

{-- day 12 --}

parseLine :: Parser Maybe (Int, [Int])
parseLine = (,) <$> integer <* symbol "<->" <*> sepBy integer (char ',')

parse :: String -> (Int, [Int])
parse = maybe (0, []) fst . runParser parseLine

collect :: [(Int, [Int])] -> [Int] -> [Int]
collect dict xs = toList . fromList . concat . (xs :) . catMaybes $ [lookup x dict | x <- xs]

day12_1 :: [String] -> Int
day12_1 input = sum . takeWhile (> 0) . zipWith (-) counts $ (0 : counts)
  where
    counts = map length components
    components = iterate (collect connections) [0]
    connections = map parse input

-- 288

{-- day 15 --}

f n x = (x * n) `mod` 2147483647

g fkt = take 40000001 . map (`mod` 65536) . iterate fkt

h fkt n = take 5000000 . map (`mod` 65536) . filter (\x -> x `mod` n == 0) . iterate fkt

day15_1, day15_2 :: Int
day15_1 = length . filter id . zipWith (==) (g (f 16807) 722) $ g (f 48271) 354
day15_2 = length . filter id . zipWith (==) (h (f 16807) 4 722) $ h (f 48271) 8 354

-- 612
-- 285

main = do
  input <- readFile "9_2017.txt"
  print . day9_1 $ input
  print . day9_2 $ input