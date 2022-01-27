module AoC2021 where

import AoCHelper (bin2Int, diff, iter, readIntLst, split')
import Data.Char (digitToInt)
import Data.List (elemIndex, group, sort, transpose, (\\))
import Data.Maybe (fromMaybe)

{-- day1 --}

day1 :: ([Int] -> [Bool]) -> [String] -> Int
day1 incFkt = length . filter id . incFkt . map read

day1_1, day1_2 :: [String] -> Int
day1_1 = day1 $ \xs -> zipWith (<) xs (tail xs)
day1_2 = day1 $ \xs -> zipWith (<) xs (tail . tail . tail $ xs)

-- 1581
-- 1618

{-- day2 --}

-- extra module Day2.hs

-- 1815044
-- 1739283308

{-- day 3 --}

ones, zeros :: [Char] -> Int
ones = length . filter (== '1')
zeros = length . filter (== '0')

day3_1, day3_2 :: [[Char]] -> Int
day3_1 input = gamma * epsilon
  where
    gamma = bin2Int lst
    epsilon = bin2Int . map not $ lst
    lst = map (\col -> ones col > zeros col) t
    t = transpose input
day3_2 input = oxygen * co2
  where
    oxygen = toInt (>=) input
    co2 = toInt (<) input
    toInt comp = bin2Int . map (== '1') . calc comp []

    calc comp bits [xs] = reverse bits ++ xs
    calc comp bits xss = calc comp (bit : bits) xss'
      where
        heads = map head xss
        bit = if ones heads `comp` zeros heads then '1' else '0'
        xss' = map tail . filter ((== bit) . head) $ xss

-- 2967914
-- 7041258

{-- day 4 --}

type Zeile = [Int]

type Matrix = [Zeile]

bingoNr :: [Int] -> Matrix -> Maybe Int
bingoNr numbers matrix = min column row
  where
    row = calc matrix
    column = calc . transpose $ matrix
    calc = minimum . map (maximum . map (`elemIndex` numbers))

day4 :: ([Maybe Int] -> Maybe Int) -> [String] -> Int
day4 minmax input = number * restSum
  where
    restSum = sum $ concat matrix \\ take (x + 1) numbers
    matrix = cards !! idx
    number = numbers !! x
    Just idx = elemIndex (Just x) bingoNrs
    Just x = minmax bingoNrs
    bingoNrs = map (bingoNr numbers) cards

    numbers = readIntLst . head $ nrs
    cards = map (map (map read . words)) cds
    nrs : cds = split' null input

day4_1, day4_2 :: [String] -> Int
day4_1 = day4 minimum
day4_2 = day4 maximum

-- 71708
-- 34726

{-- day 5 --}

-- extra module Day5.hs

-- 5147
-- 16925

{-- day 6 --}

calc :: [Int] -> [Int]
calc xs = xs' ++ replicate count 8
  where
    xs' = map fkt xs
    count = length . filter (== 0) $ xs
    fkt n = if n == 0 then 6 else pred n

calc128 :: Int -> [Int]
calc128 n = iter calc 128 [n]

fIn :: [String] -> [(Int, Int)]
fIn = map (\xs -> (head xs, length xs)) . group . sort . readIntLst . head

day6_1, day6_2 :: [String] -> Int
day6_1 = sum . map (\(x, y) -> y * (length . iter calc 80 $ [x])) . fIn
day6_2 input =
  sum $
    zipWith
      (*)
      (map snd $ fIn input)
      (map (\x -> sum $ zipWith (*) sums (map snd $ lst !! x)) [1 .. 5])
  where
    sums = map (sum . map snd) lst
    lst = map (map (\xs -> (head xs, length xs)) . group . sort . calc128) [0 .. 8]

-- 380758
-- 1710623015163

{-- day 7 --}

day7_1, day7_2 :: [String] -> Int
day7_1 input = fkt (lst !! half) lst
  where
    fkt x = sum . map (\y -> abs (y - x))
    half = length lst `div` 2
    lst = sort . readIntLst . head $ input
day7_2 input = fkt (pred average) lst
  where
    fkt x = sum . map (\y -> sumUpTo $ abs (y - x))
    sumUpTo n = (n * (n + 1)) `div` 2
    average = round ((fromIntegral . sum $ lst) / (fromIntegral . length $ lst))
    lst = readIntLst . head $ input

-- 349769
-- 99540554

{-- day 8 --}

-- extra module Day8.hs

-- 514
-- 1012272

{-- day 9 --}

day9_1 :: [String] -> Int
day9_1 input =
  sum . map sum . zz (*) m $
    map (map fromEnum) $ zz (&&) (t h m) (h m)
  where
    h m' = zz (&&) (map f m') (map g m')
    f l = zipWith (<) l (tail l) ++ [True]
    g = reverse . f . reverse
    t fkt = transpose . fkt . transpose
    zz = zipWith . zipWith
    m = map (map ((1 +) . digitToInt)) input

-- 494

{-- day 10 --}

type Stack = String

values :: [(Char, Int)]
values = [('(', 1), ('[', 2), ('{', 3), ('<', 4), (')', 3), (']', 57), ('}', 1197), ('>', 25137)]

pairs :: [(Char, Char)]
pairs = [(')', '('), (']', '['), ('}', '{'), ('>', '<')]

value :: Char -> Int
value c = fromMaybe 0 (lookup c values)

pair :: Char -> Char
pair c = fromMaybe '-' (lookup c pairs)

calculate :: Stack -> String -> (Int, Stack)
calculate stack [] = (0, stack)
calculate stack (x : xs)
  | x `elem` "([{<" = calculate (x : stack) xs
  | null stack || (head stack /= pair x) = (value x, stack)
  | otherwise = calculate (tail stack) xs

day10_1, day10_2 :: [String] -> Int
day10_1 = sum . map (fst . calculate [])
day10_2 input = values !! (length values `div` 2)
  where
    values = sort . map (f . map value) $ stacks
    f = foldl (\x y -> 5 * x + y) 0
    stacks = map snd . filter (\(x, y) -> x == 0) . map (calculate []) $ input

-- 358737
-- 4329504793

{-- day 13 --}

-- extra module Day13.hs

-- 775
-- REUPUPKR

{-- day 14 --}

-- extra module Day14.hs

-- 2703
-- 2984946368465

{-- day 25 --}

goEast :: String -> String
goEast s = init . tail . go $ last s : s ++ [head s]
  where
    go [] = []
    go ('>' : '.' : rest) = '.' : '>' : go rest
    go (x : rest) = x : go rest

goSouth :: String -> String
goSouth s = init . tail . go $ last s : s ++ [head s]
  where
    go [] = []
    go ('v' : '.' : rest) = '.' : 'v' : go rest
    go (x : rest) = x : go rest

move :: [String] -> [String]
move = transpose . map goSouth . transpose . map goEast

day25_1 input = fmap (+ 1) . elemIndex True . zipWith (==) moves $ tail moves
  where
    moves = iterate move input

-- 321

main = do
  print . day1_1 . lines =<< readFile "day1.input"
  print . day1_2 . lines =<< readFile "day1.input"