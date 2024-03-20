{-# LANGUAGE TupleSections #-}

module AoC2022 where

import AoCHelper (Pair, both, odds, chunksOf, iter, splitBy, splitWith, windowed, (.+.), (.-.))
import MPCAS (Parser, runParser, anyChar, bracketed, char, intP, sepBy, string, symbol, token, upper)

import Control.Applicative (many, some, (<|>))
import Data.Bool (bool)
import Data.Char (digitToInt, intToDigit, isDigit, isLetter)
import Data.Function (on)
import Data.List (elemIndex, intersect, sort, sortOn, tails, transpose)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, toList, intersection, union, (\\))

{-- day1 --}

day1 :: [String] -> [Integer]
day1 = map (sum . map read) . splitBy (== "")

day1_1, day1_2 :: [String] -> Integer
day1_1 = maximum . day1
day1_2 = sum . take 3 . reverse . sort . day1

--  65912
-- 195625

{-- day2 --}

day2_1, day2_2 :: [String] -> Int
day2_1 = sum . map f
  where
    f "A X" = 3 + 1
    f "A Y" = 6 + 2
    f "A Z" = 0 + 3
    f "B X" = 0 + 1
    f "B Y" = 3 + 2
    f "B Z" = 6 + 3
    f "C X" = 6 + 1
    f "C Y" = 0 + 2
    f "C Z" = 3 + 3
    f _ = 0
day2_2 = sum . map f
  where
    f "A X" = 3 + 0
    f "A Y" = 1 + 3
    f "A Z" = 2 + 6
    f "B X" = 1 + 0
    f "B Y" = 2 + 3
    f "B Z" = 3 + 6
    f "C X" = 2 + 0
    f "C Y" = 3 + 3
    f "C Z" = 1 + 6
    f _ = 0

-- 11873
-- 12014

{-- day 3 --}

charToInt :: Char -> Int
charToInt c = fromMaybe 0 . elemIndex c $ ' ' : ['a' .. 'z'] ++ ['A' .. 'Z']

half :: String -> [String]
half s = let (x, y) = splitAt (length s `div` 2) s in [x, y]

day3 :: [[String]] -> Int
day3 = sum . map (charToInt . head . foldr1 intersect)

day3_1, day3_2 :: [String] -> Int
day3_1 = day3 . map half
day3_2 = day3 . chunksOf 3

-- 8176
-- 2689

{-- day 4 --}

parseDay4 :: String -> Pair (Pair Int)
parseDay4 = both (both read) . both (splitWith '-') . splitWith ','

day4 :: (Pair (Pair Int) -> Bool) -> [String] -> Int
day4 f = sum . map (fromEnum . f . parseDay4)

day4_1, day4_2 :: [String] -> Int
day4_1 = day4 f
  where
    f (i1, i2) = i1 `contains` i2 || i2 `contains` i1
    (x1, y1) `contains` (x2, y2) = x1 <= x2 && y1 >= y2
day4_2 = day4 f
  where
    f (i1, i2) = not $ disjunct i1 i2
    (x1, y1) `disjunct` (x2, y2) = y1 < x2 || y2 < x1

-- 507
-- 897

{-- day 5 --}

parseMove :: String -> (Int, Int, Int)
parseMove s = let [count, from, to] = take 3 . map read . odds . words $ s in 
              (count, from, to)

move :: (String -> String) -> (Int, Int, Int) -> [String] -> [String]
move order (count, from, to) stacks = zipWith f [0 ..] stacks
  where
    toMove = order . take count $ stacks !! from
    f n
      | n == from = drop count
      | n == to = (toMove ++)
      | otherwise = id

day5 :: (String -> String) -> [String] -> [Char]
day5 order input = map head . tail . foldl (flip (move order)) stacks $ moves
  where
    [x, y] = splitBy (== "") input
    moves = map parseMove y
    stacks = ("" :) . filter (not . null) . map (filter isLetter) $ transpose x

day5_1, day5_2 :: [String] -> [Char]
day5_1 = day5 reverse
day5_2 = day5 id

-- JRVNHHCSJ
-- GNFBSBJLH

{-- day 6 --}

day6 :: Int -> [String] -> Int
day6 n = (+ n) . length . takeWhile ((< n) . length . fromList) . windowed n . head

day6_1, day6_2 :: [String] -> Int
day6_1 = day6 4
day6_2 = day6 14

-- 1833
-- 3425

{-- day 7 --}

-- extra module Day7.hs

-- 1582412
-- 3696336

{-- day 8 --}

day8 :: ([Int] -> a) -> (a -> a -> a) -> [String] -> [[a]]
day8 g bop input = zipWith (zipWith bop) rows cols
  where
    ns = map (map digitToInt) input
    f = map g . init . tails
    h xs = zipWith bop (f xs) (rev f xs)
    rows = map h ns
    cols = trans (map h) ns
    rev g = reverse . g . reverse
    trans g = transpose . g . transpose

day8_1, day8_2 :: [String] -> Int
day8_1 = sum . concatMap (map fromEnum) . day8 g (||)
  where
    g [] = False
    g (x : xs) = all (x >) xs
day8_2 = maximum . map maximum . day8 g (*)
  where
    g [] = 0
    g [x] = 0
    g (x : xs) = (+ 1) . fromMaybe (length xs - 1) $ elemIndex False . map (< x) $ xs

--   1684
-- 486540

{-- day 9 --}

data Motion = L | R | U | D deriving (Show, Read)

headMotion :: Motion -> Pair Int -> Pair Int
headMotion x = (.+.) (case x of
                        L -> (-1,  0)
                        R -> ( 1,  0)
                        U -> ( 0,  1)
                        D -> ( 0, -1)
                     )

tailMotion :: Pair Int -> Pair Int -> Pair Int
tailMotion h t
  | abs (x * y) == 1 = t
  | abs x + abs y > 1 = t .+. both signum (x, y)
  | otherwise = t
  where (x, y) = h .-. t

headPos :: [Motion] -> [Pair Int]
headPos = scanl (flip headMotion) (0, 0)

tailPos :: [Pair Int] -> [Pair Int]
tailPos = scanl (flip tailMotion) (0, 0)

day9 :: Int -> [String] -> Int
day9 n = length . fromList . tps . headPos . concatMap motion
  where
    motion s = let [x, y] = words s in replicate (read y) (read x)
    tps = foldr (.) id $ replicate n tailPos

day9_1, day9_2 :: [String] -> Int
day9_1 = day9 1
day9_2 = day9 9

-- 5930
-- 2443

{-- day 10 --}

data Instr = NoOp | Add Int

parseInstr :: [String] -> Instr
parseInstr ["addx", v] = Add (read v)
parseInstr _ = NoOp

evalInstr :: Instr -> Pair Int -> Pair Int
evalInstr (Add z) (x, y) = (x + 2, y + z)
evalInstr NoOp (x, y) = (x + 1, y)

valueAt :: [Pair Int] -> Int -> Int
valueAt values n = snd . last $ takeWhile ((< n) . fst) values

day10 :: [String] -> (Int -> Int)
day10 = valueAt . scanl (flip evalInstr) (0, 1) . map (parseInstr . words)

day10_1 :: [String] -> Int
day10_1 input = sum . map ((*) <*> day10 input) $ [20, 60, 100, 140, 180, 220]

day10_2 :: [String] -> [String]
day10_2 input = chunksOf 40 pixels
  where
    sprite x = [x - 1, x, x + 1]
    f n = (n `mod` 40) `elem` (sprite . day10 input . succ $ n)
    pixels = map (bool '.' '#' . f) [0 .. 239]

-- 12540
-- FECZELHE

{-- day 11 --}

-- extra module Day11.hs

-- 112815
-- 25738411485

{-- day 12 --}

-- extra module Day12.hs

-- 468
-- 459

{-- day 13 --}

data Tree a
  = Leaf a
  | Tree [Tree a]
  deriving (Show)

instance Eq a => Eq (Tree a) where
  (==) :: Eq a => Tree a -> Tree a -> Bool
  Leaf x == Leaf y = x == y
  Leaf x == Tree y = [Leaf x] == y
  Tree x == Leaf y = x == [Leaf y]
  Tree x == Tree y = x == y

instance Ord a => Ord (Tree a) where
  compare :: Ord a => Tree a -> Tree a -> Ordering
  compare (Leaf x) (Leaf y) = compare x y
  compare (Leaf x) (Tree y) = compare [Leaf x] y
  compare (Tree x) (Leaf y) = compare x [Leaf y]
  compare (Tree x) (Tree y) = compare x y

treeP :: Parser Maybe (Tree Int)
treeP = Leaf <$> intP <|> Tree <$> bracketed (sepBy treeP (char ','))

parseDay13 :: String -> Tree Int
parseDay13 = maybe (Tree []) fst . runParser treeP

day13_1, day13_2 :: [String] -> Int
day13_1 = sum . zipWith (*) [1 ..] . map f . splitBy (== "")
  where
    f [x, y] = fromEnum . ((<=) `on` parseDay13) x $ y
    f _ = 0
day13_2 input = pos 2 * pos 6
  where
    lst = sort . (tree 6 :) . (tree 2 :) . map parseDay13 . concat $ splitBy (== "") input
    pos n = (1 +) . fromMaybe 0 . elemIndex (tree n) $ lst
    tree n = Tree [Tree [Leaf n]]

--  4821
-- 21890

{-- day 14 --}

-- extra module Day14.hs

-- 618
-- 26358

{-- day 15 --}

sb :: Parser Maybe (Pair (Pair Int))
sb = do
  sx <- string "Sensor at x=" *> intP
  sy <- string ", y=" *> intP
  bx <- string ": closest beacon is at x=" *> intP
  by <- string ", y=" *> intP
  return ((sx, sy), (bx, by))

parseSB :: String -> Pair (Pair Int)
parseSB = maybe ((0, 0), (0, 0)) fst . runParser sb

concatIntervals :: [Pair Int] -> [Pair Int]
concatIntervals intervals = concatIntervals' (sort intervals) []
  where
    concatIntervals' [] ys = ys
    concatIntervals' [x] ys = x : ys
    concatIntervals' (x@(lx, rx) : y@(ly, ry) : xs) ys
      | rx < ly = concatIntervals' (y : xs) (x : ys)
      | ry < rx = concatIntervals' (x : xs) ys
      | otherwise = concatIntervals' ((lx, ry) : xs) ys

day15 :: [String] -> [(Pair Int, Int)]
day15 input = zip (map fst sbs) . map r $ sbs
  where
    sbs = map parseSB input
    r = uncurry (+) . both abs . uncurry (.-.)

day15_1, day15_2 :: [String] -> Int
day15_1 = pred . sum . map length . concatIntervals . map interval . filter ((>= 0) . g) . day15
  where
    length (x, y) = y - x + 1
    interval sr@((sx, _), _) = (sx - g sr, sx + g sr)
    g ((_, sy), r) = r - abs (sy - y)
    y = 20000000

day15_2 input = x * 4000000 + y
  where
    sds = day15 input
    (desc1, _) = findDeltaOfTwo . f (+) $ sds -- y + x = b  / /
    (_, asc2) = findDeltaOfTwo . f (-) $ sds -- y - x = b  \ \
    y = (desc1 + asc2) `div` 2 -- /\     Schnittpunkt der ersten / und zweiten \
    x = y - asc2 + 1 --   \    y in \ einsetzen -> x-Wert des Schnittpunktes -> + 1
    f op = sort . concatMap (\((sx, sy), d) -> [sy `op` sx + d, sy `op` sx - d])
    findDeltaOfTwo zss = head . filter (\(b1, b2) -> b2 - b1 == 2) . zip zss $ tail zss

-- 5832528
---13360899249595


{-- day 16 --}

valveP, maybeS :: Parser Maybe String
valveP = token $ some upper
maybeS = many $ char 's'

nodeP :: Parser Maybe (String, Int, [String])
nodeP = do
  fv <- symbol "Valve" *> valveP
  r <- symbol "has flow rate=" *> intP
  symbol "; tunnel" <* maybeS
  symbol "lead" <* maybeS
  symbol "to valve" <* maybeS
  tvs <- sepBy valveP (symbol ",")
  return (fv, r, tvs)

parseV :: String -> (String, Int, [String])
parseV = maybe ("", 0, []) fst . runParser nodeP

day16_1 = sortOn (\(_, v, _) -> - v) . map parseV



{-- day 17 --}

hline, cross, corner, vline, block, blanks :: [String]
hline = ["  @@@@ "]
cross = [ "   @   "
        , "  @@@  "
        , "   @   "]
corner = [ "    @  "
         , "    @  "
         , "  @@@  "]
vline = [ "  @    "
        , "  @    "
        , "  @    "
        , "  @    "]
block = [ "  @@   "
        , "  @@   "]
blanks = [ "       "
         , "       "
         , "       "]

-- push < or >
-- fall down
-- until down not possible

-- starting
-- 2 from the left, 3 to the highest buttom rock

toMotion :: Char -> Motion
toMotion '<' = L
toMotion '>' = R
toMotion _ = L

day17_1 = pushes
  where
    pushes = map toMotion . head
    shapes = cycle [hline, cross, corner, vline, block]



{-- day 18 --}

neighbors :: Pair Int -> [Int] -> [[Int]]
neighbors (cMin, cMax) xs =
  filter (all (>= cMin)) . filter (all (<= cMax))
    . map (zipWith (+) xs) $ [[0, 0, 1], [0, 0, -1], 
                              [0, 1, 0], [0, -1, 0], 
                              [1, 0, 0], [-1, 0, 0]]

day18 :: [String] -> (Set [Int], [Int] -> Int, Pair Int)
day18 input = (cubes, nbs, (cMin, cMax))
  where
    cubes = fromList . map (map read . splitBy (== ',')) $ input
    nbs = length . intersection cubes . fromList . neighbors (cMin, cMax)
    cMin = minimum . concat $ toList cubes
    cMax = maximum . concat $ toList cubes

day18_1, day18_2 :: [String] -> Int
day18_1 input = let (cubes, nbs, _) = day18 input in 
                sum . map ((6 -) . nbs) $ toList cubes

day18_2 input = day18_1 input - sum (map nbs inner)
  where
    (cubes, nbs, (cMin, cMax)) = day18 input
    border = [[x, y, z] | let i = [cMin, cMax], x <- i, y <- i, z <- i, [x, y, z] `notElem` cubes]
    nextNbs xs = (fromList . concatMap (neighbors (cMin, cMax)) $ xs) \\ cubes
    outer = foldr union cubes . take 25 . iterate nextNbs $ fromList border
    inner = [[x, y, z] | let i = [cMin .. cMax], x <- i, y <- i, z <- i, [x, y, z] `notElem` outer]

-- 3576
-- 2066



{-- day 19 --}

eachP :: Parser Maybe Int
eachP = do
  anyChar
  symbol "Each"
  symbol "ore" <|> symbol "clay" <|> symbol "obsidian" <|> symbol "geode"
  symbol "robot costs" *> intP <* symbol "ore"

robotP :: Parser Maybe (Int, Int, Pair Int, Pair Int)
robotP = do
  symbol "Blueprint" *> intP
  ore <- eachP
  clay <- eachP
  obs <- (,) <$> eachP <* symbol "and" <*> intP <* symbol "clay"
  geo <- (,) <$> eachP <* symbol "and" <*> intP <* symbol "obsidian"
  anyChar
  return (ore, clay, obs, geo)

parseRobot :: String -> (Int, Int, (Int, Int), (Int, Int))
parseRobot = maybe (0, 0, (0, 0), (0, 0)) fst . runParser robotP

day19_1 = map parseRobot



{-- day 20 --}

setBetweenAt :: Int -> a -> [a] -> [a]
setBetweenAt n x xs = let (l, r) = splitAt n xs in l ++ x : r

step :: Int -> [Pair Int] -> [Pair Int]
step k ns -- = setBetweenAt ak' ak (after ++ before)
  | ak' > m = setBetweenAt (ak' - m) ak before ++ after
  | otherwise = before ++ setBetweenAt ak' ak after
  where
    (before, ak : after) = span ((/= k) . fst) ns
    m = length after
    l' = length ns - 1
    ak' = flip mod l' $ snd ak

day20 :: Int -> Int -> [String] -> Int
day20 magic rounds input = sum . map (result !!) $ [1000, 2000, 3000]
  where
    result = dropWhile (/= 0) result' ++ result'
    result' = map snd . iter rounds f $ lst
    f = foldr ((.) . step) id [l', l' -1 .. 0]
    l' = fst $ last lst
    lst = zip [0..] . map ((* magic) . read) $ input

day20_1, day20_2 :: [String] -> Int
day20_1 = day20 1 1
day20_2 = day20 811589153 10

-- 8721
-- 831878881825

{-- day 21 --}

-- extra module Day21.hs

-- 194501589693264
-- 3887609741189

{-- day 22 --}

-- extra module Day22.hs

-- 3590
-- 86382

{-- day 23 --}

data Direction = N | S | W | E deriving Show

neighbours :: Pair Int -> [[Pair Int]]
neighbours (x, y) = map (map ((x, y) .+.)) 
  [[(-1, -1), (0, -1), (1, -1)],
   [(-1,  0),          (1,  0)],
   [(-1,  1), (0,  1), (1,  1)]]

lookDir :: Direction -> Pair Int -> [Pair Int]
lookDir d (x, y) = map ((x, y) .+.) 
                   . (case d of
                        N -> head
                        S -> last
                        W -> map head
                        E -> map last) . neighbours $ (x, y)

day23_1 = elves
  where
    elves = fromList . concat
        . zipWith (\x ys -> map ( ,x) ys) [1..]
        . map (map fst . filter ((== '#') . snd) . zip [1..])


{-- day 24 --}

-- extra module Day24.hs

-- 251
-- 758

{-- day 25 --}

char2Int :: Char -> Int
char2Int c
  | isDigit c = digitToInt c
  | c == '-' = -1
  | c == '=' = -2
  | otherwise = 0

int2Char :: Int -> Char
int2Char n
  | n >= 0 = intToDigit n
  | n == -1 = '-'
  | n == -2 = '='
  | otherwise = ' '

snafu2Int :: String -> Int
snafu2Int = foldl (\x y -> 5 * x + char2Int y) 0

int2SNAFU :: Int -> String
int2SNAFU = reverse . map int2Char . convertDec2Quint

convertDec2Quint :: Int -> [Int]
convertDec2Quint 0 = []
convertDec2Quint n = r' : convertDec2Quint q'
  where
    (q, r) = quotRem n 5
    (q', r') = bool (q, r) (q + 1, r - 5) $ r > 2

day25_1 :: [String] -> String
day25_1 = int2SNAFU . sum . map snafu2Int

-- 2=222-2---22=1=--1-2


main :: IO ()
main = do
  input <- readFile "day1.input"
  print . day1_1 . lines $ input