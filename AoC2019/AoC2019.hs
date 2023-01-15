{-# LANGUAGE OverloadedStrings #-}

module AoC2019 where

import AoCHelper (iter, split')
import Data.List (group, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, fromList, intersection, toList, union)
import Data.Text (pack, replace, splitAt, unpack)
import MPCAS (Parser, integer, runParser, symbol)

{-- day 1 --}

fuel :: Int -> Int
fuel n = (n `div` 3) - 2

iterFuel :: Int -> Int
iterFuel = sum . takeWhile (> 0) . tail . iterate fuel

day1_1, day1_2 :: [String] -> Int
day1_1 = sum . map (fuel . read)
day1_2 = sum . map (iterFuel . read)

-- 3406432
-- 5106777

{-- day 3 --}

type Cmd = (Char, Int)

toCmd :: String -> Cmd
toCmd (c : r) = (c, read r)
toCmd _ = ('U', 0)

buildWay :: ((Int, Int), Set (Int, Int)) -> Cmd -> ((Int, Int), Set (Int, Int))
buildWay ((x, y), s) cmd = (p, s')
  where
    p = case cmd of
      ('U', c) -> (x, y + c)
      ('D', c) -> (x, y - c)
      ('R', c) -> (x + c, y)
      ('L', c) -> (x - c, y)
      _ -> (x, y)
    s' =
      s `union` case cmd of
        ('U', c) -> fromList [(x, y + d) | d <- [1 .. c]]
        ('D', c) -> fromList [(x, y - d) | d <- [1 .. c]]
        ('R', c) -> fromList [(x + d, y) | d <- [1 .. c]]
        ('L', c) -> fromList [(x - d, y) | d <- [1 .. c]]
        _ -> empty

day3_1 :: [String] -> Int
day3_1 input = minimum . map (\(x, y) -> abs x + abs y) . toList $ cutting
  where
    cutting = (snd . head $ points) `intersection` (snd . last $ points)
    points = map (foldl buildWay ((0, 0), empty) . (map toCmd . split' (== ','))) input

-- 209

{-- day 4 --}

increase :: String -> Bool
increase s = not . or . zipWith (>) s $ tail s

day4 :: ([Int] -> Bool) -> Int
day4 f = length . filter f . map (map length . group) . filter increase . map show $ [271973 .. 785961]

day4_1, day4_2 :: Int
day4_1 = day4 (any (> 1))
day4_2 = day4 (elem 2)

-- 925
-- 607

{-- day 6 --}

day6_1 :: [String] -> Int
day6_1 input = sum . map orbits $ orbiters
  where
    orbiters = map fst orbitMap
    orbitMap = map (toPair . split' (== ')')) input
    toPair [x, y] = (y, x)
    toPair _ = ("", "")
    orbits "COM" = 0
    orbits x =
      let Just y = lookup x orbitMap
       in 1 + orbits y

-- 139597

{-- day 8 --}

countChar :: String -> Char -> Int
countChar s c = length . filter (== c) $ s

counts :: String -> (Int, Int, Int)
counts s = (countChar s '0', countChar s '1', countChar s '2')

day8_1 :: String -> Int
day8_1 input = o * t
  where
    (_, o, t) = minimum . map counts . chunksOf (25 * 6) $ input

day8_2 :: String -> String
day8_2 = unlines . chunksOf 25 . unpack . replace "0" " " . pack . map (head . filter (/= '2')) . transpose . chunksOf (25 * 6)

-- 2210
-- CGEGE

{-- day 12 --}

type Point = (Int, Int, Int)

extractPosition :: Parser Maybe Point
extractPosition = do
  symbol "<x="
  x <- integer
  symbol ", y="
  y <- integer
  symbol ", z="
  z <- integer
  symbol ">"
  return (x, y, z)

parsePosition :: String -> Point
parsePosition = maybe (0, 0, 0) fst . runParser extractPosition

diff :: Point -> Point -> Point
diff (x1, y1, z1) (x2, y2, z2) = (signum (x2 - x1), signum (y2 - y1), signum (z2 - z1))

add :: Point -> Point -> Point
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

velocityDiff :: Point -> [Point] -> (Int, Int, Int)
velocityDiff x = foldr (add . diff x) (0, 0, 0)

abssum :: Point -> Int
abssum (x, y, z) = abs x + abs y + abs z

energy :: ((Int, Int, Int), Point) -> Int
energy (v, p) = abssum v * abssum p

step :: [((Int, Int, Int), Point)] -> [((Int, Int, Int), Point)]
step xs = foldr f [] xs
  where
    f x ys =
      let vd = velocityDiff (snd x) (map snd xs)
       in (add (fst x) vd, add vd $ uncurry add x) : ys

day12_1 :: [String] -> Int
day12_1 = sum . map energy . iter step 1000 . zip (repeat (0, 0, 0)) . map parsePosition

-- 14809

main = do
  input <- readFile "3_2019.txt"
  print . day3_1 . lines $ input
