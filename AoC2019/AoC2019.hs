{-# LANGUAGE OverloadedStrings #-}

module AoC2019 where

import AoCHelper (chunksOf, iter, readIntLst, splitBy, splitWith, Pair)
import Data.List (group, groupBy, transpose, sort, elemIndex, (\\))
import Data.Map as Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Set as Set (Set, empty, fromList, intersection, toList, union)
import Data.Text (pack, replace, splitAt, unpack)
import MPCAS (Parser, integer, runParser, symbol)
import Data.Graph (graphFromEdges, vertices, topSort, reverseTopSort)
import Data.Tuple (swap)
import Data.Function (on)

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

{-- day 2 --}

go :: Int -> Map Int Int -> Int
go n prog
  | Map.lookup n prog == Just 99 = fromJust $ Map.lookup 0 prog
  | otherwise =
    let op = if Map.lookup n prog == Just 1 then (+) else (*)
        source1 = fromJust $ Map.lookup (fromJust $ Map.lookup (n + 1) prog) prog
        source2 = fromJust $ Map.lookup (fromJust $ Map.lookup (n + 2) prog) prog
        target = fromJust $ Map.lookup (n + 3) prog
     in go (n + 4) $ insert target (op source1 source2) prog

day2 :: Int -> Int -> [String] -> Int
day2 x y = go 0 . insert 2 y . insert 1 x . Map.fromList . zip [0 ..] . readIntLst . head

day2_1 :: [String] -> Int
day2_1 = day2 12 2

day2_2 :: [String] -> Int
day2_2 input = head [100 * x + y | x <- [0..99], y <- [0..99], day2 x y input == 19690720]

-- 3760627
-- 7195

{-- day 3 --}

type Cmd = (Char, Int)

toCmd :: String -> Cmd
toCmd (c : r) = (c, read r)
toCmd _ = ('U', 0)

buildWay :: (Pair Int, Set (Pair Int)) -> Cmd -> (Pair Int, Set (Pair Int))
buildWay ((x, y), s) cmd = (p, s')
  where
    p = case cmd of
      ('U', c) -> (x, y + c)
      ('D', c) -> (x, y - c)
      ('R', c) -> (x + c, y)
      ('L', c) -> (x - c, y)
      _ -> (x, y)
    s' = s `union` case cmd of
      ('U', c) -> Set.fromList [(x, y + d) | d <- [1 .. c]]
      ('D', c) -> Set.fromList [(x, y - d) | d <- [1 .. c]]
      ('R', c) -> Set.fromList [(x + d, y) | d <- [1 .. c]]
      ('L', c) -> Set.fromList [(x - d, y) | d <- [1 .. c]]
      _ -> empty

goWay :: (Pair Int, [[Pair Int]]) -> Cmd -> (Pair Int, [[Pair Int]])
goWay ((x, y), l) cmd = (p, l')
  where
    p = case cmd of
      ('U', c) -> (x, y + c)
      ('D', c) -> (x, y - c)
      ('R', c) -> (x + c, y)
      ('L', c) -> (x - c, y)
      _ -> (x, y)
    l' = (case cmd of
      ('U', c) -> [(x, y + d) | d <- [0 .. c]]
      ('D', c) -> [(x, y - d) | d <- [0 .. c]]
      ('R', c) -> [(x + d, y) | d <- [0 .. c]]
      ('L', c) -> [(x - d, y) | d <- [0 .. c]]
      _ -> []) : l

count :: [[Pair Int]] -> Pair Int -> Int
count [] _ = 0
count (xs : xss) p =
  case elemIndex p xs of
    Nothing -> length xs - 1 + count xss p
    Just x -> x

day3 :: [String] -> ([[Cmd]], [Pair Int])
day3 input = (cmds, crossing) 
  where
    cmds = map (map toCmd . splitBy (== ',')) input
    points = map (foldl buildWay ((0, 0), empty)) cmds
    crossing = toList $ ((intersection `on` snd) <$> head <*> last) points

day3_1, day3_2 :: [String] -> Int
day3_1 = minimum . map (((+) `on` abs) <$> fst <*> snd) . snd . day3

day3_2 input = minimum $ map ((+) <$> count w1 <*> count w2) crossing
  where
    (cmds, crossing) = day3 input
    [w1, w2] = map (reverse . snd . foldl goWay ((0,0), [])) cmds

-- 209
-- 43258

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

day6 :: [String] -> ([Pair String], String -> [Maybe String])
day6 input = (orbitMap, searchWay)
  where
    orbitMap = map (swap . splitWith ')') input
    back node = Prelude.lookup node orbitMap
    searchWay node = takeWhile isJust . iterate (back =<<) $ pure node

day6_1, day6_2 :: [String] -> Int
day6_1 input = sum . map (orbits . fst) $ orbitMap
  where
    (orbitMap, searchWay) = day6 input
    orbits "COM" = 0
    orbits node = pred . length $ searchWay node
    
day6_2 input = length (san \\ you) + length (you \\ san) - 2
  where
    (_, searchWay) = day6 input
    you = searchWay "YOU"
    san = searchWay "SAN"
    
-- 139597
-- 286

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
day12_1 = sum . map energy . iter 1000 step . map (((0, 0, 0),) . parsePosition)

-- 14809

main = do
  input <- readFile "6_2019.txt"
  print . day6_1 . lines $ input
  print . day6_2 . lines $ input
