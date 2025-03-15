module AoC2017 where

import AoCHelper (Grid, Pair, splitBy, both, findChar, (.+.), (@), fixPt)
import Control.Applicative (Alternative (many, (<|>)))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt, isLetter)
import Data.Graph (components, graphFromEdges, topSort, Graph, Vertex)
import Data.List (sort, elemIndex, elemIndices, transpose, sortOn, groupBy, group)
import Data.Map as Map (adjust, lookup, fromList)
import Data.Maybe (fromMaybe, fromJust)
import Data.Set as Set (Set, fromList, size, toList)
import MPCAS (Parser, anyChar, char, integer, runParser, sepBy, symbol, parenthezised, taged, identifier, int)
import Data.Text.Internal.Read (IParser(runP))
import Data.Function (on)

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
day2 fkt = sum . fkt . map (map read . splitBy (== '\t'))

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
day4 = length . filter ((==) <$> size . Set.fromList <*> length)

day4_1, day4_2 :: [String] -> Int
day4_1 = day4 . map words
day4_2 = day4 . map (map sort . words)

-- 466
-- 251

{-- day 5 --}

day5_1 :: [String] -> Int
day5_1 input = f 0 0 ns
  where
    ns = Map.fromList . zip [0 ..] $ map read input
    f n k xs = let jmp = Map.lookup n xs
                in case jmp of
                  Nothing -> k
                  Just j -> f (n + j) (k + 1) $ Map.adjust (+1) n xs

-- part 2 is to slow with the same approach
-- part 2 solved in python

-- the imperative approach is so much faster! What I have to do in Haskell to get the same speed?

-- 373543
-- 27502966

{-- day 7 --}

graphParser1 :: Parser Maybe (Int, String, [String])
graphParser1 = do
  node <- identifier
  value <- parenthezised int
  symbol "->"
  neighbors <- sepBy identifier (char ',')
  return (value, node, neighbors)

graphParser2 :: Parser Maybe(Int, String, [String])
graphParser2 = do
  node <- identifier
  value <- parenthezised int
  return (value, node, [])

parseGraph :: String -> (Int, String, [String])
parseGraph = maybe (0, "", []) fst . runParser (graphParser1 <|> graphParser2)

day7 :: [String] -> (Graph, Vertex -> (Int, String, [String]), String -> Maybe Vertex)
day7 input = graphFromEdges $ map parseGraph input

day7_1 :: [String] -> String
day7_1 input = let (graph, nodeFromVertex, vertexFromKey) = day7 input
                   (value, key, neighbors) = nodeFromVertex . head $ topSort graph
                in key

day7_2 input = map (g . nodeFromVertex) $ topSort graph
  where
    (graph, nodeFromVertex, vertexFromKey) = day7 input
    g (v, k, n) = (k, map (f . nodeFromVertex . fromJust . vertexFromKey) n)
    f :: (Int, String, [String]) -> Int
    f (v, k, n)
       | null n = v
       | otherwise = v + (sum . map (f . nodeFromVertex . fromJust . vertexFromKey)) n

-- dtacyn
-- 521


{-- day 8 --}

-- Part 1 solved in Python

-- 3612
-- 3818

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
day11 = fromMaybe [] . mapM (`Prelude.lookup` symDict) . splitBy (== ',')

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

day12 input = ((,) <$> length . head <*> length) $ components g
  where
    (g, n, v) = graphFromEdges $ map (f . parse) input
    f (x, xs) = (x, x, xs)

-- 288
-- 211

{-- day 15 --}

f n x = (x * n) `mod` 2147483647

g fkt = take 40000001 . map (`mod` 65536) . iterate fkt

h fkt n = take 5000000 . map (`mod` 65536) . filter (\x -> x `mod` n == 0) . iterate fkt

day15_1, day15_2 :: Int
day15_1 = length . filter id . zipWith (==) (g (f 16807) 722) $ g (f 48271) 354
day15_2 = length . filter id . zipWith (==) (h (f 16807) 4 722) $ h (f 48271) 8 354

-- 612
-- 285

{-- day 19 --}

data Direction = N | S | E | W deriving (Show, Eq)

dirToInt :: Direction -> Pair Int
dirToInt N = (-1, 0)
dirToInt S = (1, 0)
dirToInt E = (0, 1)
dirToInt W = (0, -1)

step :: Grid Char -> (Pair Int, Direction) -> (Pair Int, Direction)
step grid (pos, dir)
  | grid @ pos == '+' =
      if dir `elem` [N, S] then
        if grid @ (pos .+. dirToInt E) == ' '
          then (pos .+. dirToInt W, W) else (pos .+. dirToInt E, E)
      else {- dir `elem` [E, W] -}
        if grid @ (pos .+. dirToInt N) == ' '
          then (pos .+. dirToInt S, S) else (pos .+. dirToInt N, N)
  | otherwise =
      if grid @ (pos .+. dirToInt dir) == ' '
        then ((-1, -1), dir) else (pos .+. dirToInt dir, dir)

day19 grid = (filter isLetter $ map (grid @) path, length path)
  where
    start = head $ findChar grid '|'
    path = takeWhile ((== (True, True)) . both (>= 0)) . map fst $ iterate (step grid) (start, S)

-- GSXDIPWTU
-- 16100

{-- day 20 --}

data Particle = Particle {p, v, a :: [Int]} deriving (Show)

particleParser :: Parser Maybe Particle
particleParser = Particle <$> f "p=" <*> f ", v=" <*> f ", a="
  where
    f s = symbol s *> taged (sepBy integer (char ','))

parseParticle :: String -> Particle
parseParticle = maybe (Particle [] [] []) fst . runParser particleParser

move :: Particle -> Particle
move (Particle p v a) = let v' = zipWith (+) v a
                            p' = zipWith (+) p v'
                         in Particle p' v' a

day20_1 = (zipWith elemIndex =<< map minimum) . transpose . map (f . parseParticle)
  where f = map (sum . map abs . p) . iterate move

day20_2 = map length . iterate f . map parseParticle
  where
    f = map move . concat . filter ((== 1) . length) . groupBy ((==) `on` p) . sortOn p

-- 308
-- 504


main :: IO ()
main = do
  input <- readFile "7_2017.txt"
  print . day7_2 . lines $ input
  -- print . day20_2 . lines $ input