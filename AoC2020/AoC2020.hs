import AoCHelper (split', bin2Int)
import Data.List (tails, sort, elemIndex, intersect, (\\))


{-- day 1 --}

day11 lst = [ x * y | x <- lst, y <- lst,
                      x <= y, x + y == 2020 ]
day12 lst = [ x * y * z | x <- lst, y <- lst, x <= y, 
                          z <- lst, y <= z, x + y + z == 2020 ]
day1 input = map (($ lines input) . (. map read)) [day11, day12]

-- 319531
-- 244300320


{-- day 2 --}

-- extra module Day2.hs

-- 614
-- 354


{-- day 3 --}

steps :: Int -> [Int]
steps n = map (`mod` 31) [0, n..]

isTree :: (String, Int) -> Bool
isTree (s, n) = s !! n == '#'

half :: [a] -> [a]
half (x : y : xs) = x : half xs
half lst = lst

day31 lst n = length . filter isTree $ zip lst (steps n)
day3_1 lst = day31 lst 3
day3_2 lst = day31 (half lst) 1 * (product . map (day31 lst) $ [1, 3, 5, 7])

-- 223
-- 3517401300


{-- day 4 --}

-- extra module Day 4.hs

-- 219
-- 127


{-- day 5 --}

seatID :: (String, String) -> Int
seatID (rCode, cCode) = row * 8 + column
  where (row, column) = (bin2Int . map (== 'B') $ rCode, bin2Int . map (== 'R') $ cCode)

code2Nr :: [String] -> [Int]
code2Nr = map (seatID . splitAt 7)

day5_1 = maximum . code2Nr
day5_2 lst = (sorted !! n) + 1
  where Just n = elemIndex (-2) $ zipWith (-) sorted (tail sorted)
        sorted = sort . code2Nr $ lst

-- 880
-- 731


{-- day 6 --}

abc = ['a'..'z']

letterCount :: String -> Int
letterCount str = 26 - length (abc \\ str)

day6 mapping = sum . map mapping . split' null
day6_1, day6_2 :: [String] -> Int
day6_1 = day6 $ letterCount . concat
day6_2 = day6 $ length . foldr1 intersect

-- 6521
-- 3305


{-- day 7 --}

-- extra module Day7.hs

-- 121
-- 3805


{-- day 8 --}

-- extra module Day8.hs

-- 1727
-- 552


{-- day 9 --}

hasSum :: [Int] -> Bool
hasSum numbers = not . null $ [ x | x <- elements, y <- elements, x < y, x + y == sum ]
  where elements = init numbers
        sum = last numbers

day9_1 = last . head . dropWhile hasSum . map (take 26) . tails . map read
day9_2 lst =  minimum sumLst + maximum sumLst
  where sumLst = take (idx + 1) l
        Just idx = elemIndex part1 (scanl1 (+) l)
        l = head . filter (elem part1 . scanl1 (+)) . tails . map read $ lst
        part1 = day9_1 lst

-- 248131121
-- 31580383


{-- day 10 --}

-- extra module Day10.hs

-- 1755
-- 4049565169664


{-- day 11 --}

-- implemented in Squeak Smalltalk

-- 2194
-- 1944


{-- day 12 --}

-- extra module Day12.hs

-- 757
-- 51249


{-- day 13 --}

-- extra module Day13.hs

-- 2305
-- 552612234243498


{-- day 14 --}

-- extra module Day14.hs

-- 14925946402938
-- 3706820676200


{-- day 15 --}

-- extra module Day15.hs

-- 468
-- 1801753


{-- day 16 --}

-- extra module Day16.hs

-- 22000
-- 410460648673


{-- day 17 --}

-- implemented in Squeak Smalltalk

-- 301
-- 2424


{-- day 18 --}

-- in Pharo Smalltalk using Smacc this is trivial
-- for Haskell look at extra module Day18.hs
-- also very nice is parsing in Haskell -> Day18MPCAS.hs

-- 21022630974613
-- 169899524778212


{-- day 19 --}

-- extra module Day19.hs

-- 176
-- 352


{-- day 20 --}

-- extra module Day20.hs

-- 174206308298779
-- 2409


{-- day 21 --}

-- extra module Day21.hs

-- 2584
-- fqhpsl,zxncg,clzpsl,zbbnj,jkgbvlxh,dzqc,ppj,glzb


{-- day 22 --}

-- extra module Day22.hs

-- 35005
-- 32751


{-- day 23 --}

-- implemented in Squeak Smalltalk

-- 43896725
-- 2911418906


{-- day 24 --}

-- extra module Day24.hs

-- 388
-- 4002

{-- day 25 --}

modul = 20201227

transform :: Integer -> (Integer -> Integer)
transform n x = mod (n * x) modul

day25_1 lst = (pkc ^ m) `mod` modul
  where Just n = elemIndex pkc trans7
        Just m = elemIndex pkd trans7
        trans7 = iterate (transform 7) 1
        (pkc, pkd) = (read a, read b)
        [a, b] = lst
day25_2 = const "nothing to solve!-)"

-- 18293391
-- nothing to solve!-)


main = do
  print . day1 =<< readFile "day1.input"