module AoC2024 where

import AoCHelper (Pair, between, both, cross, evens, findChar, iter, odds, readIntLst, splitBy, splitWith, (.+.), (.-.), (@), Grid)
import Control.Monad.Trans.State.Lazy (StateT, evalState, get, modify)
import Data.Bits (Bits (shiftL, shiftR, xor, (.&.)))
import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.Functor.Identity (Identity)
import Data.List (elemIndex, group, groupBy, isPrefixOf, isSuffixOf, partition, sort, sortBy, transpose, (\\))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import Data.Set (empty, fromList, singleton, size, toAscList, toList)
import Data.Text (count, pack)
import MPCAS (Parser, char, ident, int, nat, runParser, sepBy, string)

{-- day 1 --}

day1 :: [String] -> Pair [Int]
day1 input = let [l, r] = transpose $ map (map read . words) input in (l, r)

day1_1, day1_2 :: [String] -> Int
day1_1 = sum . uncurry (zipWith ((abs .) . (-))) . both sort . day1
day1_2 input = let (l, r) = day1 input in sum [sum [y | y <- r, x == y] | x <- l]

-- 2970687
-- 23963899

{-- day 2 --}

p :: [Int] -> Bool
p = (&&) <$> all (`between` (-3, 3)) <*> ((||) <$> all (> 0) <*> all (< 0))

deltas :: [Int] -> [Int]
deltas ns = zipWith (-) ns $ tail ns

day2 :: [String] -> [[Int]]
day2 = map (map read . words)

day2_1, day2_2 :: [String] -> Int
day2_1 = length . filter p . map deltas . day2
day2_2 = length . filter (any p) . map (map deltas . g) . day2
  where
    g ns = ns : [((++) <$> take (n - 1) <*> drop n) ns | n <- [1 .. length ns]]

-- 407
-- 459

{-- day 3 --}

type ParseFkt = String -> [Int]

mulParser :: Parser Maybe Int
mulParser = (*) <$> (string "mul(" *> nat) <*> (char ',' *> nat <* char ')')

parseDay3 :: ParseFkt -> ParseFkt
parseDay3 continue s
  | null s = []
  | otherwise = case runParser mulParser s of
      Just (x, y) -> x : parseDay3 continue y
      Nothing -> continue s

parseTxt1, parseTxt2 :: ParseFkt
parseTxt1 = parseDay3 (parseTxt1 . tail)
parseTxt2 = parseDay3 parseDont

parseD :: String -> ParseFkt -> ParseFkt -> ParseFkt
parseD d succ fail s
  | null s = []
  | otherwise = case runParser (string d) s of
      Just (x, y) -> succ y
      Nothing -> fail $ tail s

parseDont, parseDo :: ParseFkt
parseDont = parseD "don't()" parseDo parseTxt2
parseDo = parseD "do()" parseTxt2 parseDo

day3 :: ParseFkt -> [String] -> Int
day3 fkt = sum . fkt . concat

day3_1, day3_2 :: [String] -> Int
day3_1 = day3 parseTxt1
day3_2 = day3 parseTxt2

-- 173419328
-- 90669332

{-- day 4 --}

diagonals :: [String] -> [String]
diagonals = ((++) `on` transpose) <$> zipWith drop [0 ..] <*> map reverse . zipWith take [0 ..]

day4_1, day4_2 :: [String] -> Int
day4_1 grid = sum $ map ($ grid) [rows, cols, diags]
  where
    rows = counts
    cols = counts . transpose
    diags = ((+) `on` counts . diagonals) <*> reverse
    counts = ((+) `on` sum) <$> countX <*> countRX
    countX = map (count (pack "XMAS") . pack)
    countRX = map (count (pack "SAMX") . pack)
day4_2 grid = let len = length grid
               in length
                    [ (x, y) | (x, y) <- (,) <$> [1 .. len - 2] <*> [1 .. len - 2]
                            , grid @ (x, y) == 'A'
                            , [grid @ (x - 1, y - 1), grid @ (x, y), grid @ (x + 1, y + 1)] `elem` ["MAS", "SAM"]
                            , [grid @ (x - 1, y + 1), grid @ (x, y), grid @ (x + 1, y - 1)] `elem` ["MAS", "SAM"]
                    ]

-- 2297
-- 1745

{-- day 5 --}

mid :: [a] -> a
mid as = as !! (length as `div` 2)

day5 :: [String] -> Pair Int
day5 input = (p1, p2)
  where
    [x, y] = splitBy (== "") input
    poRules = map (both read . splitWith '|') x
    pages = map readIntLst y
    (goods, bads) = partition ((==) <*> sortBy (compWith poRules)) pages
    p1 = sum $ map mid goods
    p2 = sum $ map (mid . sortBy (compWith poRules)) bads

compWith :: [Pair Int] -> Int -> Int -> Ordering
compWith poRules x y
  | (x, y) `elem` poRules = LT
  | (y, x) `elem` poRules = GT
  | otherwise = EQ

-- 7365
-- 5770

{-- day 6 --}

-- extra module Day6.hs

-- 4696
-- 1443 (solved in Python)

{-- day 7 --}

checkOp :: Int -> [Int] -> Bool
checkOp n [] = False
checkOp n [m] = n == m
checkOp n (x : y : ns) = checkOp n (x + y : ns) || checkOp n (x * y : ns)

checkOp' :: Int -> [Int] -> Bool
checkOp' n ns = checkOpFromBehind' n $ reverse ns

{-- naive approach:
checkOp' n [] = False
checkOp' n [m] = n == m
checkOp' n (x : y : ns)
  | n < x || n < y = False
  | otherwise =
      checkOp' n (x + y : ns) ||
      checkOp' n (x * y : ns) ||
      let n |.| m = read (show n ++ show m) in
      checkOp' n (x |.| y : ns)
--}

checkOpFromBehind' :: Int -> [Int] -> Bool
checkOpFromBehind' m [] = False
checkOpFromBehind' m [n] = m == n
checkOpFromBehind' m (n : ns) =
  m >= n && checkOpFromBehind' (m - n) ns
    || m `mod` n == 0 && checkOpFromBehind' (m `div` n) ns
    || let n' = show n; m' = show m
        in n' `isSuffixOf` m' && checkOpFromBehind' (m `div` 10 ^ length n') ns

day7 :: (Int -> [Int] -> Bool) -> [String] -> Int
day7 check input = sum . map fst $ filter (uncurry check) parsed
  where
    parsed = map (f . splitWith ':') input
    f (x, y) = (read x, map read $ words y)

day7_1, day7_2 :: [String] -> Int
day7_1 = day7 checkOp
day7_2 = day7 checkOp'

-- 1153997401072
-- 97902809384118

{-- day 8 --}

day8 :: [String] -> Pair Int
day8 grid = both part (part1, part2)
  where
    part fkt = size . fromList . concatMap (fkt . pairs . findChar grid) $ ['0' .. '9'] <> ['a' .. 'z'] <> ['A' .. 'Z']
    part1 = filter inRange . map f
    part2 = concatMap f'
    f (p1, p2) = p1 .+. (p1 .-. p2)
    f' (p1, p2) = takeWhile inRange $ iterate (.+. (p1 .-. p2)) p1
    pairs ps = [(p1, p2) | p1 <- ps, p2 <- ps, p1 /= p2]
    l = length grid
    inRange = (== (True, True)) . both (`between` (0, l - 1))

-- 259
-- 927

{-- day 9 --}

day9_1 :: [String] -> Int
day9_1 input = sum . zipWith (*) [0 ..] . take l $ move files spaces lst revLst
  where
    revLst = reverse lst
    lst = concatMap (uncurry replicate) ns
    ns = zip files [0 ..]
    sizes = map digitToInt $ head input
    l = sum files
    files = evens sizes
    spaces = odds sizes

day9_2 :: [String] -> Int
day9_2 input = sum . zipWith g [0 ..] . reverse $ f (reverse files) spaces
  where
    posSize = zip pos sizes
    pos = scanl (+) 0 sizes
    sizes = map digitToInt $ head input
    files = evens posSize
    spaces = odds posSize
    g value (pos, size) = (value * size * (2 * pos + size - 1)) `div` 2

f :: [Pair Int] -> [Pair Int] -> [Pair Int]
f [] _ = []
f ((pos, size) : files) spaces =
  let (l, r) = break (\(x, y) -> x < pos && y >= size) spaces
   in if null r
        then (pos, size) : f files spaces
        else
          let (pos', size') = head r
              spaces' =
                if size == size'
                  then l ++ tail r
                  else l ++ (pos' + size, size' - size) : tail r
           in (pos', size) : f files spaces'

move :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
move (x : xs) (y : ys) ints revInts =
  take x ints
    ++ take y revInts
    ++ move xs ys (drop x ints) (drop y revInts)

-- 6390180901651
-- 6412390114238

{-- day 10 --}

day10 :: [String] -> Pair Int
day10 input = (p length, p $ sum . map snd)
  where
    p aggregate = sum $ map ((aggregate . twoToNine . step '1') . (,1)) zeros

    zeros = findChar grid '0'

    addDot = map (reverse . ('.' :) . reverse . ('.' :))
    grid = transpose . addDot . transpose . addDot $ input
    l = length grid

    step c (p, h) = map (,h) . filter ((== c) . (grid @)) $ map (p .+.) cross
    compress = map ((,) <$> fst . head <*> sum . map snd) . groupBy ((==) `on` fst) . sort

    twoToNine = foldr (flip (.)) id steps
    steps = map (\c -> compress . concatMap (step c)) ['2' .. '9']

-- 482
-- 1094

{-- day 11 --}

day11 :: [String] -> Pair Int
day11 input = both solve (25, 75)
  where
    solve = sum . map snd . (iterate (compress . concatMap g) (map (,1) ns) !!)
    compress = map ((,) <$> fst . head <*> sum . map snd) . groupBy ((==) `on` fst) . sort
    g (n, h) = map (,h) $ step n

    step n
      | n == 0 = [1]
      | even . length $ show n = let (x, y) = half $ show n in [read x, read y]
      | otherwise = [n * 2024]

    half xs = splitAt (length xs `div` 2) xs
    ns = map read . words $ head input

-- 220722
-- 261952051690787

{-- day 12 --}

-- extra module Day12.hs

-- 1573474

{-- day 13 --}

day13 :: ([Pair Int] -> [Pair Int]) -> [String] -> Int
day13 trans = sum . map (((+) <$> (3 *) . fst <*> snd) . solveLGS) . parse
  where
    parse = map (trans . map (both (read . filter isDigit) . splitWith ',')) . splitBy (== "")

day13_1, day13_2 :: [String] -> Int
day13_1 = day13 trans
day13_2 = day13 trans'

trans, trans' :: [Pair Int] -> [Pair Int]
trans [(a, c), (b, d), (b1, b2)] = [(a, b), (c, d), (b1, b2)]
trans' [(a, c), (b, d), (b1, b2)] = [(a, b), (c, d), both (10_000_000_000_000 +) (b1, b2)]

{--
    ax + by = b1
    cx + dy = b2
--}

solveLGS :: [Pair Int] -> Pair Int
solveLGS [(a, b), (c, d), (b1, b2)]
  | dt == 0 = (0, 0)
  | dx `mod` dt /= 0 = (0, 0)
  | dy `mod` dt /= 0 = (0, 0)
  | otherwise = (dx `div` dt, dy `div` dt)
  where
    dt = det (a, b) (c, d)
    dx = det (b1, b) (b2, d)
    dy = det (a, b1) (c, b2)

det :: Pair Int -> Pair Int -> Int
det (a, b) (c, d) = a * d - b * c

-- 29187
-- 99968222587852

{-- day 14 --}

-- extra module Day14.hs

-- 214109808
-- 7687

{-- day 15 --}

-- extra module Day15.hs

-- 1465523

{-- day 16 --}

-- use Dijkstra's algorithm

day16_1 grid = (start, end)
  where
    start = head $ findChar grid 'S'
    end = head $ findChar grid 'E'

{-- day 17 --}

-- solved with Python

-- 7,3,5,7,5,7,4,3,0
-- 105734774294938

{-- day 18 --}

-- extra module Day18.hs

-- 408
-- 45,16

{-- day 19 --}

memoize :: Ord a => (a -> StateT (Map a b) Identity b) -> a -> StateT (Map a b) Identity b
memoize f a = do
  cache <- get
  case Data.Map.lookup a cache of
    Just b -> return b
    Nothing -> do
      b <- f a
      modify (insert a b)
      return b

run :: Ord a => (a -> StateT (Map a b) Identity b) -> a -> b
run f a = evalState (memoize f a) Data.Map.empty

anyM :: (Traversable t, Monad f) => (a -> f Bool) -> t a -> f Bool
anyM pred = (or <$>) <$> mapM pred

day19 :: [String] -> Pair Int
day19 input = (p1, p2)
  where
    -- p1 = length $ filter (run canBuild) designs
    p1 = length $ filter (> 0) counts
    p2 = sum counts
    counts = map (run countBuild) designs

    designs = drop 2 input
    patterns = maybe [] fst . runParser patternParser $ head input

    candidates word = filter (`isPrefixOf` word) patterns

    canBuild word
      | null word = return True
      | otherwise = anyM (memoize canBuild . flip drop word . length) $ filter (`isPrefixOf` word) patterns
    countBuild word
      | null word = return 1
      | otherwise = sum <$> mapM (memoize countBuild . flip drop word . length) (filter (`isPrefixOf` word) patterns)

patternParser :: Parser Maybe [String]
patternParser = sepBy ident (string ", ")

-- 313
-- 666491493769758

{-- day 20 --}

day20_1 :: Grid Char -> Int
day20_1 input = length . filter (>= 100) . map (dist <$> head <*> last) . filter ((== 2) . length) $ map nbs candidates
  where
    path = start : findWay start end []

    start = head $ findChar input 'S'
    end = head $ findChar input 'E'
    walls = findChar input '#'

    l = length input
    candidates = [pair | pair <- walls, (True, True) == both (`between` (1, l - 2)) pair]

    nbs p = filter ((`elem` ".SE") . (input @)) $ map (p .+.) cross
    dist p q = subtract 2 . abs $ fromJust (elemIndex p path) - fromJust (elemIndex q path)

    findWay target goal way
      | target == goal = way
      | otherwise =
          let next = head $ nbs target \\ take 2 way
           in findWay next goal (next : way)

-- 1378

{-- day 21 --}

-- extra module Day21.hs

{-- day 22 --}

day22_1 :: [String] -> Integer
day22_1 = sum . map (iter 2000 g . read)
  where
    g = mixAndPrune (`shiftL` 11) . mixAndPrune (`shiftR` 5) . mixAndPrune (`shiftL` 6)
    mixAndPrune f s = (xor <*> f) s .&. 16777215

-- 14392541715

{-- day 23 --}

day23_1 :: [String] -> Int
day23_1 input = (sum . map f) doubleTEdges + (sum . map f) (tEdges \\ doubleTEdges) `div` 2
  where
    edges = map (splitWith '-') input
    vertices = toList . fromList $ [x | (x, y) <- edges] ++ [y | (x, y) <- edges]
    ts = [t | t <- vertices, "t" `isPrefixOf` t]

    doubleTEdges = [(x, y) | (x, y) <- tEdges, x `elem` ts && y `elem` ts]
    tEdges = [(x, y) | (x, y) <- edges, x `elem` ts || y `elem` ts]

    f (x, y) = length
                  [ z | let others = edges \\ doubleTEdges
                      , z <- vertices
                      , (x, z) `elem` others || (z, x) `elem` others
                      , (y, z) `elem` others || (z, y) `elem` others
                  ]

-- 1344

{-- day 24 --}

-- part 1 in an extra module Day24.hs

-- 42410633905894

{-- day 25 --}

day25 :: [String] -> Int
day25 input = length . filter id $ fit <$> map (h head) locks <*> map (h last) keys
  where
    keylocks = splitBy (== "") input
    (locks, keys) = partition ((== "#####") . head) keylocks

    h f = map (length . f . group) . transpose
    fit = (all (<= 7) .) . zipWith (+)

-- 2993

main = do
  input <- readFile "day11.input"
  print . day11 . lines $ input
