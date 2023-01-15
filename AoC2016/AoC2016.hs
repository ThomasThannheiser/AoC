module AoC2016 where

import AoCHelper (chunksOf, split')
import Control.Applicative (Alternative ((<|>)))
import Data.List (group, groupBy, sort, transpose, (\\))
import MPCAS (Parser, char, identifier, runParser, satisfy, sepBy)

{-- day1 --}

day1 input = (e - w, s - n)
  where
    [e, s, w, n] = map (sum . map snd) . groupBy (\x y -> fst x == fst y) . sort . zip firsts $ map (read . tail) input
    firsts = map ((`mod` 4) . ($ 0)) . scanl1 (.) . map ((\x -> if x == 'R' then (+ 1) else (\y -> y - 1)) . head) $ input

toInput :: [Char] -> [[Char]]
toInput = map tail . split' (== ',') . (' ' :)

day1_1 :: (Num a, Ord a, Read a) => [Char] -> a
day1_1 lst = abs x + abs y
  where
    (x, y) = day1 . toInput $ lst

-- 300

{-- day 2 --}

go :: Int -> Char -> Int
go 1 'R' = 2
go 1 'D' = 4
go 2 'L' = 1
go 2 'R' = 3
go 2 'D' = 5
go 3 'L' = 2
go 3 'D' = 6
go 4 'U' = 1
go 4 'D' = 7
go 4 'R' = 5
go 5 'U' = 2
go 5 'D' = 8
go 5 'L' = 4
go 5 'R' = 6
go 6 'U' = 3
go 6 'D' = 9
go 6 'L' = 5
go 7 'U' = 4
go 7 'R' = 8
go 8 'U' = 5
go 8 'L' = 7
go 8 'R' = 9
go 9 'U' = 6
go 9 'L' = 8
go n _ = n

go' :: Char -> Char -> Char
go' '1' 'D' = '3'
go' '2' 'D' = '6'
go' '2' 'R' = '3'
go' '3' 'U' = '1'
go' '3' 'D' = '7'
go' '3' 'L' = '2'
go' '3' 'R' = '4'
go' '4' 'D' = '8'
go' '4' 'L' = '3'
go' '5' 'R' = '6'
go' '6' 'U' = '2'
go' '6' 'D' = 'A'
go' '6' 'L' = '5'
go' '6' 'R' = '7'
go' '7' 'U' = '3'
go' '7' 'D' = 'B'
go' '7' 'L' = '6'
go' '7' 'R' = '8'
go' '8' 'U' = '4'
go' '8' 'D' = 'C'
go' '8' 'L' = '7'
go' '8' 'R' = '9'
go' '9' 'L' = '8'
go' 'A' 'U' = '6'
go' 'A' 'R' = 'B'
go' 'B' 'U' = '7'
go' 'B' 'D' = 'D'
go' 'B' 'L' = 'A'
go' 'B' 'R' = 'C'
go' 'C' 'U' = '8'
go' 'C' 'L' = 'B'
go' 'D' 'U' = 'B'
go' c _ = c

day2_1, day2_2 :: [String] -> String
day2_1 = concatMap show . tail . scanl (foldl go) 5
day2_2 = tail . scanl (foldl go') '5'

-- 74921
-- A6B35

{-- day 3 --}

readLines :: [String] -> [[Int]]
readLines = map (map read . words)

countTriangles :: [[Int]] -> Int
countTriangles = length . filter (\[x, y, z] -> x + y > z) . map sort

day3_1, day3_2 :: [String] -> Int
day3_1 = countTriangles . readLines
day3_2 = countTriangles . chunksOf 3 . concat . transpose . readLines

-- 869
-- 1544

{-- day 4 --}

-- extra module AoC2016Day4.hs

-- 137896
-- 501

{-- day 5 --}

-- implemented in Squeak Smalltalk, cause using md5 is simpler there

-- 1a3099aa
-- 694190cd

{-- day 6 --}

day6 :: Ord b => Int -> [[b]] -> [b]
day6 n =
  map (snd . head . filter ((== n) . fst) . map (\xs -> (length xs, head xs)) . group . sort)
    . transpose

day6_1, day6_2 :: [String] -> String
day6_1 = day6 26
day6_2 = day6 24

-- "afwlyyyq"
-- "bhkzekao"

{-- day 7 --}

extractParts :: Parser Maybe [String]
extractParts = sepBy identifier (char '[' <|> char ']')

parseIP :: String -> [String]
parseIP = maybe [] fst . runParser extractParts

hasDoubleReverse :: String -> Bool
hasDoubleReverse xs@(x1 : x2 : x3 : x4 : r) = (x1 /= x2) && (x1 == x4) && (x2 == x3) || hasDoubleReverse (tail xs)
hasDoubleReverse _ = False

hasIPTrue :: [Bool] -> Bool
hasIPTrue (x : y : rest) = y || hasIPTrue rest
hasIPTrue _ = False

day7_1 :: [String] -> Int
day7_1 = length . filter (not . hasIPTrue) . filter or . map (map hasDoubleReverse . parseIP)

-- 105

{-- Chinese remainder theorem! --}

day15 :: Integer
day15 = mod (sum $ zipWith (*) modLst (map calc miLst)) m
  where
    miLst = [17, 7, 19, 5, 3, 13, 11]
    modLst = [2, 2, 5, -1, -1, -2, -4]
    m = product miLst
    calc mi = (head . filter ((== 0) . (`mod` mi)) $ iterate (+ negate (div m mi)) 1) - 1

-- 317371
-- 2080951

{-- day 16 --}

toBool :: Char -> Bool
toBool '1' = True
toBool _ = False

toBit :: Bool -> Char
toBit True = '1'
toBit False = '0'

initial :: [Bool]
initial = map toBool "11011110011011101"

reverseComplement :: [Bool] -> [Bool]
reverseComplement = map not . reverse

step :: [Bool] -> [Bool]
step xs = xs ++ False : reverseComplement xs

generate :: Int -> [Bool] -> [Bool]
generate n = head . dropWhile (\xs -> length xs < n) . iterate step

check :: [Bool] -> [Bool]
check xs = if odd . length $ xs then xs else check . shrink $ xs
  where
    shrink (x : y : rest) = (x == y) : shrink rest
    shrink _ = []

day16 :: Int -> [Char]
day16 n = map toBit . check . take n . generate n $ initial

day16_1, day16_2 :: [Char]
day16_1 = day16 272
day16_2 = day16 35651584 -- works but performance may be better

-- 00000100100001100
-- 00011010100010010

{-- day 18 --}

parseLine :: String -> [Bool]
parseLine = map (== '^')

isTrap :: (Bool, Bool) -> Bool
isTrap (x, z) = x && not z || not x && z

nextLine :: [Bool] -> [Bool]
nextLine bs = map isTrap tuples
  where
    tuples = zip bs' (tail . tail $ bs')
    bs' = False : bs ++ [False]

day18 :: Int -> String -> Int
day18 n = sum . map (length . filter not) . take n . iterate nextLine . parseLine

day18_1, day18_2 :: String -> Int
day18_1 = day18 40
day18_2 = day18 400000

-- 2013
-- 20006289

{-- day 19 --}

day19 :: Integer
day19 = 3017957

-- day19_1 = 2^21 + 920805 => (Josephus-Problem) result = 2 * 920805 + 1 = 1841611
-- day19_2 = 3^13 + 1423634 => if X = 3^n + Z => result = 3^n + 2*Z else result = X
--                  1423634 <= 2^13 => result = 1423634

-- 1841611
-- 1423634

{-- day 20 --}

day20_1 :: [String] -> [(Int, Int)]
day20_1 input = sort intervalls
  where
    intervalls = map (((,) <$> head <*> last) . map read . split' (== '-')) input

-- 14975795

main = do
  print . day3_1 . lines =<< readFile "3_2016.txt"
  print . day3_2 . lines =<< readFile "3_2016.txt"
