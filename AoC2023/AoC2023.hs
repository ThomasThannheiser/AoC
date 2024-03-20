module AoC2023 where

import AoCHelper (Grid, Pair, between, both, gridAt, chunksOf, nbh, skipUpTo, splitBy, splitWith, (.+.))
import MPCAS (runParser, symbol, upper)

import Control.Applicative (Alternative (many))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isDigit, ord)
import Data.Function (on)
import Data.List (elemIndex, elemIndices, group, groupBy, intercalate, intersect, isPrefixOf, isSuffixOf, partition, sort, sortOn, tails, transpose, unfoldr)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)

{- day 1 -}

day1_1, day1_2 :: [String] -> Int
day1_1 = sum . map (read . ((:) <$> head <*> pure . last) . filter isDigit)
day1_2 = sum . map ((+) <$> ((10 *) . toFirstNumber) <*> toLastNumber)

toFirstNumber, toLastNumber :: String -> Int
toFirstNumber = toNumber isPrefixOf tail
toLastNumber = toNumber isSuffixOf init

toNumber :: (String -> String -> Bool) -> (String -> String) -> String -> Int
toNumber isPartOf cutChar str
  | n == 0 = toNumber isPartOf cutChar $ cutChar str
  | otherwise = n
  where
    n = (`mod` 10) . succ . length . takeWhile not . map (`isPartOf` str) $ numbers
    numbers = map show [1 .. 9] ++ " " : ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- 53651
-- 53894

{- day 2 -}

day2_1, day2_2 :: [String] -> Int
day2_1 = sum . map fst . filter snd . zip [1 ..] . map (inLimit . parseColors)
  where
    inLimit [b, g, r] = b <= 14 && g <= 13 && r <= 12
day2_2 = sum . map (product . parseColors)

parseColors :: String -> [Int]
parseColors str = map (maximum . map (read . snd)) groups
  where
    groups = groupBy ((==) `on` fst) . sort $ concat bgrs
    bgrs = map (map (swap . splitWith ' ' . tail) . splitBy (== ',')) sets
    sets = splitBy (== ';') $ skipUpTo ':' str

-- 3099
-- 72970

{- day 3 -}

forRowsAndColumns :: ([a] -> [a]) -> [[a]] -> [[a]]
forRowsAndColumns f = transpose . map f . transpose . map f

day3_1 :: Grid Char -> Int
day3_1 input = sum $ map (read . map fst) ns
  where
    ns = filter (any snd) . splitBy (not . isDigit . fst) $ zip (concat input) (concat bs)
    bs = forRowsAndColumns propagate . map (map markSymbol) $ input
    markSymbol '.' = False
    markSymbol c = not $ isDigit c

propagate :: [Bool] -> [Bool]
propagate [] = []
propagate [b] = [b]
propagate (True : False : bs) = True : True : propagate bs
propagate (_ : b : bs) = b : propagate (b : bs)

day3_2 :: Grid Char -> Int
day3_2 input = sum . map (product . map (read . snd)) $ filter ((== 2) . length) groups
  where
    groups = groupBy ((==) `on` fst) . sort $ map ((,) <$> head . filter isJust . map snd <*> map fst) ns
    ns = filter (any (isJust . snd)) . splitBy (not . isDigit . fst) $ zip (concat input) (concat bs)
    bs = forRowsAndColumns propagate' . chunksOf l . markSymbol 1 $ concat input
    l = length $ head input
    markSymbol n [] = []
    markSymbol n (x : xs)
      | x == '*' = Just n : markSymbol (n + 1) xs
      | otherwise = Nothing : markSymbol n xs

propagate' :: [Maybe Int] -> [Maybe Int]
propagate' [] = []
propagate' [n] = [n]
propagate' (Just n : Nothing : bs) = Just n : Just n : propagate' bs
propagate' (_ : n : bs) = n : propagate' (n : bs)

-- 535235
-- 79844424

{- day 4 -}

parseCard :: String -> Pair [Int]
parseCard = both readInts . splitWith '|' . snd . splitWith ':'
  where
    readInts = map read . filter (/= "") . splitBy (== ' ')

countWinnings :: [String] -> [Int]
countWinnings = map (length . (intersect <$> fst <*> snd) . parseCard)

day4_1, day4_2 :: [String] -> Int
day4_1 = sum . map ((2 ^) . pred) . filter (/= 0) . countWinnings
day4_2 = sum . map snd . unfoldr f . map (,1) . countWinnings
  where
    f [] = Nothing
    f ((x, y) : ps) = Just ((x, y), let (as, bs) = splitAt x ps in map (.+. (0, y)) as ++ bs)

-- 24733
-- 5422730

{- day 5 -}

day5_1, day5_2 :: [String] -> Int
day5_1 input = minimum $ map fkt seeds
  where
    (seeds, maps) = parseMaps input
    f [d, s, l] x
      | x `between` (s, s + l - 1) = Just $ d + x - s
      | otherwise = Nothing
    g x m =
      let v = mapMaybe (($ x) . f) m
       in if null v then x else head v
    fkt x = foldl g x maps
day5_2 input = minimum . map head . fst . foldl h (intervals, []) $ maps
  where
    (seeds, maps) = parseMaps input
    intervals = chunksOf 2 seeds
    g (uneffected, effected) dsl =
      let pairs = map (cutIntervals dsl) uneffected
       in (concatMap fst pairs, concatMap snd pairs ++ effected)
    h interval dsls = (uncurry (++) $ foldl g interval dsls, [])

{- OMG!-O interval arithmetic O-!
   Cutting dsl with an interval, calculate d for the intersection part
   uneffected intervals to the left, effected intervals to the right
-}
cutIntervals :: [Int] -> [Int] -> Pair [[Int]]
cutIntervals [d, s, l] [x, l']
  | y' < s = ([[x, l']], [])
  | x < s && y' <= e = ([[x, s - x]], [[d, y' - s + 1]])
  | x < s = ([[x, s - x], [s + l, y' - s - l + 1]], [[d, l]])
  | y' <= e = ([], [[x + d - s, l']])
  | x <= e = ([[e + 1, y' - e]], [[x + d - s, e - x + 1]])
  | otherwise = ([[x, l']], [])
  where
    y' = x + l' - 1
    e = s + l - 1

parseMaps :: [String] -> ([Int], [[[Int]]])
parseMaps input = (seeds, maps)
  where
    lst = splitBy (== "") input
    seeds = readInts . skipUpTo ':' . head $ head lst
    maps = map (map readInts . tail) $ tail lst
    readInts = map read . splitBy (== ' ')

-- 111627841
-- 69323688

{- day 6 -}

day6_1 :: [String] -> Int
day6_1 = product . map c . parseRaces
  where
    f (l, m) t = t * (l - t) > m
    c (l, m) = length $ filter (f (l, m)) [1 .. pred l]

day6_2 :: [String] -> Integer
day6_2 input = g - h + 1
  where
    ints = parseRaces' input
    l = fromIntegral $ head ints
    m = fromIntegral $ last ints
    g = floor $ (l + sqrt d) / 2
    h = ceiling $ (l - sqrt d) / 2
    d = l * l - 4 * m

parseRaces :: [String] -> [Pair Int]
parseRaces = (zip <$> head <*> last) . map (readInts . skipUpTo ':')
  where
    readInts = map read . filter (/= "") . splitBy (== ' ')

parseRaces' :: [String] -> [Integer]
parseRaces' = map (read . filter isDigit . skipUpTo ':')

-- 2344708
-- 30125202

{- day 7 -}

day7_1, day7_2 :: [String] -> Int
day7_1 = day7 hand2Ints1
day7_2 = day7 hand2Ints2

day7 :: (String -> [Int]) -> [String] -> Int
day7 toInts = sum . zipWith (*) [1 ..] . map snd . sortOn (toInts . fst) . map parseCards

parseCards :: String -> (String, Int)
parseCards = second read . splitWith ' '

hand2Ints, hand2Ints1, hand2Ints2 :: String -> [Int]
hand2Ints = reverse . sort . map length . group . sort
hand2Ints1 = (++) <$> hand2Ints <*> map char2Int
hand2Ints2 s =
  let (x, y) = partition (== 'J') s
   in addJoker (length x) (hand2Ints y) ++ map char2Int' s

addJoker :: Int -> [Int] -> [Int]
addJoker x [] = [x]
addJoker x (y : xs) = x + y : xs

char2Int :: Char -> Int
char2Int c
  | isDigit c = digitToInt c
  | c == 'T' = 10
  | c == 'J' = 11
  | c == 'Q' = 12
  | c == 'K' = 13
  | c == 'A' = 14
  | otherwise = 0

char2Int' :: Char -> Int
char2Int' 'J' = 1
char2Int' c = char2Int c

-- 248105065
-- 249515436

{- day 8 -}

day8 :: [String] -> (String, Map String (Pair String))
day8 input = (lrs, network)
  where
    strs = splitBy (== "") input
    lrs = cycle . head $ head strs
    network = Map.fromList . map parseNW $ last strs

goLR :: Map String (Pair String) -> String -> Char -> String
goLR network s c = f c . fromJust $ Map.lookup s network
  where
    f 'L' = fst
    f 'R' = snd

day8_1, day8_2 :: [String] -> Int
day8_1 input = length . takeWhile (/= "ZZZ") $ scanl (goLR network) start lrs
  where
    (lrs, network) = day8 input
    start = head $ Map.keys network
day8_2 input = foldr (lcm . g) 1 (filter ((== 'A') . (!! 2)) $ Map.keys network)
  where
    (lrs, network) = day8 input
    g x = length . takeWhile ((/= 'Z') . (!! 2)) $ scanl (goLR network) x lrs

parseNW :: String -> (String, Pair String)
parseNW = maybe ("", ("", "")) fst . runParser parser
  where
    parser = (,) <$> many upper <* symbol "="
                 <*> ((,) <$> (symbol "(" *> many upper <* symbol ",") <*> many upper <* symbol ")")

-- 16697
-- 10668805667831

{- day 9 -}

day9_1, day9_2 :: [String] -> Int
day9_1 = sum . map (calcNext . parseInts)
day9_2 = sum . map (calcNext . reverse . parseInts)

parseInts :: String -> [Int]
parseInts = map read . splitBy (== ' ')

calcNext :: [Int] -> Int
calcNext = sum . reverse . map last . takeWhile (any (/= 0)) . iterate diffs
  where
    diffs = zipWith (-) <$> tail <*> id

-- 1921197370
-- 1124

{- day 10 -}

-- extra module Day10.hs

-- 6820
-- 337

{- day 11 -}

day11 :: Grid Char -> Int
day11 grid = sum [dist x y | x <- galaxies, y <- galaxies, x < y]
  where
    galaxies = sort [(y, x) | y <- [0 .. pred m], x <- [0 .. pred n], gridAt grid (y, x) == '#']
    dist (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)
    m = length grid
    n = length $ head grid

day11_1, day11_2 :: Grid Char -> Int
day11_1 = day11 . transpose . expand . transpose . expand
day11_2 grid = normalDist + 999999 * deltaExpand
  where
    deltaExpand = expandedDist - normalDist
    expandedDist = day11_1 grid
    normalDist = day11 grid

isEmpty :: String -> Bool
isEmpty = all (== '.')

expand :: Grid Char -> Grid Char
expand [] = []
expand (xs : xss)
  | isEmpty xs = xs : xs : expand xss
  | otherwise = xs : expand xss

-- 9734203
-- 568914596391

{- day 12 -}

-- extra module Day12.hs, Day12State.hs

-- 6935
-- 3920437278260

{- day 13 -}

day13 :: Eq b => b -> ([String] -> Int -> b) -> [String] -> Int
day13 b fkt = sum . map ((\(x, y) -> 100 * x + y) . both (maybe 0 succ) . g b) . splitBy (== "")
  where
    g b aas = both (h . ($ aas)) (id, transpose)
    h aas = elemIndex b $ map (fkt aas) [1 .. pred $ length aas]

day13_1, day13_2 :: [String] -> Int
day13_1 = day13 True hasMirrorAt
day13_2 = day13 1 countDiffs

hasMirrorAt :: Eq a => [a] -> Int -> Bool
hasMirrorAt as n =
  let (l', r) = mirrorSplit as n
   in l' `isPrefixOf` r || r `isPrefixOf` l'

countDiffs :: Eq c => [[c]] -> Int -> Int
countDiffs ccs n =
  let (l', r) = mirrorSplit ccs n
   in length . filter not . concat $ zipWith (zipWith (==)) l' r

mirrorSplit :: [a] -> Int -> ([a], [a])
mirrorSplit as n = let (l, r) = splitAt n as in (reverse l, r)

-- 28895
-- 31603

{- day 14 -}

day14_1, day14_2 :: Grid Char -> Int
day14_1 = sumUp . rollNorth
day14_2 input = sumUp $ (foldl (.) id $ replicate it rollNWSE) dup
  where
    lst = repeatUntilDuplicate rollNWSE input
    dup = rollNWSE $ head lst
    cycleLength = succ . fromJust . elemIndex dup $ lst
    it = (1000000000 - length lst) `mod` cycleLength

rollWest, rollNorth, rollEast, rollSouth, rollNWSE :: Grid Char -> Grid Char
rollWest = map (intercalate "#" . map (reverse . sort) . splitBy (== '#') . ('#' :))
rollNorth = transpose . rollWest . transpose
rollEast = map reverse . rollWest . map reverse
rollSouth = reverse . rollNorth . reverse
rollNWSE = rollEast . rollSouth . rollWest . rollNorth

sumUp :: Grid Char -> Int
sumUp grid = sum . zipWith (*) (reverse [1 .. length grid]) $ map (length . filter (== 'O')) grid

repeatUntilDuplicate :: Eq a => (a -> a) -> a -> [a]
repeatUntilDuplicate g x = go x []
  where
    go x xs = if x `elem` xs then xs else go (g x) (x : xs)

-- 113078
-- 94255

{- day 15 -}

day15_1, day15_2 :: String -> Int
day15_1 = sum . map hash . splitBy (== ',')
day15_2 = sum . map calcBox . groupBoxes . takeLast . removeBefore . parse
  where
    parse = map (((,) <$> head <*> last) . concatMap (splitBy (== '-')) . splitBy (== '=')) . splitBy (== ',')
    groupBoxes = groupBy ((==) `on` hash . fst) . sortOn (hash . fst)
    calcBox = (*) <$> succ . hash . fst . head <*> sum . zipWith (*) [1 ..] . map (read . snd)

hash :: String -> Int
hash = foldl h 0
  where
    h n c = ((n + ord c) * 17) `mod` 256

removeBefore :: [Pair String] -> [Pair String]
removeBefore xs
  | null r = l
  | otherwise = removeBefore $ filter (((fst . head) r /=) . fst) l ++ tail r
  where
    (l, r) = break (null . snd) xs

takeLast :: [Pair String] -> [Pair String]
takeLast [] = []
takeLast lst@((x, y) : xs) =
  let (l, r) = partition ((== x) . fst) lst
   in last l : takeLast r

-- 507666
-- 233537

{- day 16 -}



{- day 17 -}



{- day 18 -}

day18 :: (String -> (String, Integer)) -> [String] -> Integer
day18 parse input = perimeter + inner
  where
    coordinates = scanl f (0, 0) $ map parse input
    doubleAreal = abs . sum . zipWith g coordinates $ tail coordinates
    perimeter = sum . zipWith d coordinates $ tail coordinates
    inner = (doubleAreal + 2 - perimeter) `div` 2
    g (y1, x1) (y2, x2) = x1 * y2 - y1 * x2
    d (y1, x1) (y2, x2) = abs (x2 - x1) + abs (y2 - y1)
    f (y, x) ("R", d) = (y, x + d)
    f (y, x) ("D", d) = (y + d, x)
    f (y, x) ("L", d) = (y, x - d)
    f (y, x) ("U", d) = (y - d, x)

day18_1, day18_2 :: [String] -> Integer
day18_1 = day18 parse
  where
    parse = ((,) <$> head <*> read . last) . take 2 . splitBy (== ' ')
day18_2 = day18 parse
  where
    parse = f . take 6 . drop 2 . last . splitBy (== ' ')
    f xs =
      let (d, dir) = splitAt 5 xs
       in (g dir, read $ "0x" ++ d)
    g "0" = "R"
    g "1" = "D"
    g "2" = "L"
    g "3" = "U"

-- 74074
-- 112074045986829

{- day 19 -}

-- extra module Day19.hs

-- 6935
-- 3920437278260

{- day 20 -}



{- day 21 -}

day21 :: Grid Char -> [Pair (Set (Pair Int))]
day21 input = iterate (step input) (Set.singleton $ findS input, Set.empty)

findS :: Grid Char -> Pair Int
findS input = (`quotRem` n) . head . elemIndices 'S' $ concat input
  where
    n = length $ head input

day21_1 :: Grid Char -> Int
day21_1 = snd . both Set.size . (!! 64) . day21

step :: Grid Char -> Pair (Set (Pair Int)) -> Pair (Set (Pair Int))
step grid (last, this) = (stepover, last)
  where
    stepover = Set.union this . Set.filter isNotRock . Set.unions $ Set.map (f grid) last
    f grid z = Set.difference (Set.fromList $ nbh z) this
    isNotRock (y, x) =
      let x' = (x + 131) `mod` 131
          y' = (y + 131) `mod` 131
       in gridAt grid (y, x) /= '#'

day21_2 :: [String] -> [Int]
day21_2 input = map (\x -> fst . both Set.size $ day21 input !! x) $ (65 +) <$> take 4 [0, 131 ..]

{-- day21_2 --

  Values at: round about 500 sec. to get this!

   0 * 131 + 65 ->   3814
   1 * 131 + 65 ->  33952
   2 * 131 + 65 ->  94138
   3 * 131 + 65 -> 184372

   => f x = 15024 * x ^ 2 + 15114 * x + 3814

   f (26501365 - 65 / 131) = f 202300 = 614864614526014

-}

-- 3722
-- 614864614526014

{- day 22 -}

type Brick = [(Pair Int, Int)]

hight :: Brick -> Int
hight = snd . head

day22_1 input = sortOn hight $ foldl g [] bricks
  where bricks = sortOn hight $ map parseBrick input
        g bs b = fall b bs : bs

parseBrick :: String -> Brick
parseBrick = g . uncurry zip . both f . splitWith '~'
  where
    f = map read . splitBy (== ',')
    g [a, b, c] = [((x, y), h) | x <- [0..9], x `between` a,
                                 y <- [0..9], y `between` b,
                                 h <- [0..999], h `between` c]

fall :: Brick -> [Brick] -> Brick
fall brick bricks = last . takeWhile f $ down brick
  where f b = (null . (b `intersect`)) . concat $ bricks

down :: Brick -> [Brick]
down brick = brick :
  case hight brick of
    1 -> []
    _ -> down $ map (second pred) brick


{- day 23 -}

-- extra module Day23.hs

-- 2114


{- day 24 -}

day24_1 :: [String] -> Int
day24_1 = length . filter id . concatMap solveList . tails . map parseLines

parseLines :: String -> Pair [Int]
parseLines = both (map read . splitBy (== ',')) . splitWith '@'

det :: Num a => Pair a -> Pair a -> a
det (x1, y1) (x2, y2) = y1 * x2 - x1 * y2

solve :: Pair Int -> Pair Int -> Pair Int -> Pair Int -> Bool
solve (x1, y1) (x2, y2) (vx1, vy1) (vx2, vy2)
  | d == 0.0 = det (deltax, deltay) (vx2, vy2) == 0
  | lambda > 0.0 && mue > 0.0 = inBounds sx && inBounds sy
  | otherwise = False
  where
    d = fromIntegral $ det (vx1, vy1) (vx2, vy2)
    deltax = x2 - x1
    deltay = y2 - y1
    lambda = (/ d) . fromIntegral $ det (deltax, deltay) (vx2, vy2)
    mue = (/ d) . fromIntegral $ det (deltax, deltay) (vx1, vy1)
    sx = fromIntegral x1 + lambda * fromIntegral vx1
    sy = fromIntegral y1 + lambda * fromIntegral vy1
    inBounds v = v `between` (200000000000000, 400000000000000)

solveLines :: Pair [Int] -> Pair [Int] -> Bool
solveLines ([x1, y1, _], [vx1, vy1, _]) ([x2, y2, _], [vx2, vy2, _]) = solve (x1, y1) (x2, y2) (vx1, vy1) (vx2, vy2)

solveList :: [Pair [Int]] -> [Bool]
solveList [] = []
solveList (x : xs) = map (solveLines x) xs

-- 15107


{- day 25 -}

day25_1 input = graph
  where
    graph = map parse25 input
    start = fst $ head graph

parse25 input = (l, splitBy (== ' ') r)
  where
    (l, r) = splitWith ':' input



main :: IO ()
main = do
  input <- readFile "day22exp.input"
  print . day22_1 . lines $ input
  -- print . day22_2 . lines $ input
