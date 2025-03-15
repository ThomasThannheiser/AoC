module AoC2018 where

import AoCHelper (iter, Pair)
import Data.Char (ord, toUpper)
import Data.List (group, sort, (\\), sortBy, unfoldr, transpose, groupBy, sortOn)
import Data.Set (elemAt, fromList, difference, toList)
import MPCAS (Parser, anyChar, char, symbol, runParser, integer, parenthezised, taged)
import Data.Function (on)

{-- day 1 --}

day1 :: [String] -> [Integer]
day1 = map (read . filter (/= '+'))

day1_1 :: [String] -> Integer
day1_1 = sum . day1

-- takes a little to long!-/
day1_2 :: [String] -> Integer
day1_2 lst = findFirstDublicateIn [] $ scanl (+) 0 $ cycle . day1 $ lst
  where findFirstDublicateIn lst (x : xs)
          | x `elem` lst = x
          | otherwise = findFirstDublicateIn (x : lst) xs

-- 522
-- 73364

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

{-- day 4 --}

-- day4_1 = sort

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

extract :: Parser Maybe (Pair Char)
extract = (,) <$> (symbol "Step" *> anyChar) <*>
                  (symbol "must be finished before step" *> anyChar <* symbol "can begin.")

parse :: String -> Pair Char
parse = maybe ('.', '.') fst . runParser extract

fkt :: (String, [Pair Char]) -> (String, [Pair Char])
fkt (c, []) = (reverse c ++ sort (['A'..'Z'] \\ c), [])
fkt (c, lst) = (r : c, rest)
    where
        rest = filter ((/= r) . fst) lst
        r = minimum . toList $ ((fromList . map fst $ lst) `difference` (fromList . map snd $ lst))

day7_1 :: [String] -> String
day7_1 input =
    (map fst . iterate fkt $ ("", map parse input)) !! 26

-- FDSEGJLPKNRYOAMQIUHTCVWZXB

{-- day 8 --}

data Node = Node [Node] [Int]

emptyNode :: Node
emptyNode = Node [] []

day8 :: String -> [Node]
day8 = ints2Nodes . map read . words

day8_1, day8_2 :: String -> Int
day8_1 = sum . map sumData . day8
day8_2 = calcRootData . head . day8

sumData :: Node -> Int
sumData (Node childs ds) = sum ds + sum (map sumData childs)

calcRootData :: Node -> Int
calcRootData (Node [] ds) = sum ds
calcRootData (Node childs ds) =
  let childs' = emptyNode : childs ++ repeat emptyNode
   in sum $ map (calcRootData . (childs' !!)) ds

ints2Nodes :: [Int] -> [Node]
ints2Nodes = unfoldr g
  where
    g [] = Nothing
    g ns = Just $ makeNode ns

makeNode :: [Int] -> (Node, [Int])
makeNode ns
  | childCount == 0 = g [] rest
  | otherwise = uncurry g $ makeNodes childCount rest
  where
    ([childCount, dataCount], rest) = splitAt 2 ns
    g childs ns = let (ds, rest') = splitAt dataCount ns
                   in (Node childs ds, rest')

makeNodes :: Int -> [Int] -> ([Node], [Int])
makeNodes n ns
  | n == 0 = ([], ns)
  | otherwise = let (node, rest) = makeNode ns
                    (nodes, rest') = makeNodes (n - 1) rest
                 in (node : nodes, rest')

-- 46096
-- 24820

{-- day 10 --}

data ParticleOfLight = PoL { position :: Pair Int
                           , velocity :: Pair Int
                           } deriving (Show)

particleParser :: Parser Maybe ParticleOfLight
particleParser = PoL <$> f "position=" <*> f "velocity="
  where
    f s = symbol s *> taged ((,) <$> integer <*> (char ',' *> integer))

parseParticle :: String -> ParticleOfLight
parseParticle = maybe (PoL (0,0) (0,0)) fst . runParser particleParser

day10_1 = map sort . transpose . map (map position . iterate step . parseParticle)

step :: ParticleOfLight -> ParticleOfLight
step (PoL (x, y) (vx, vy)) = PoL (x + vx, y + vy) (vx, vy)


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
  input <- readFile "10_2018.txt"
  print . day10_1 . lines $ input
  -- print . day8_2 $ input
