module AoC2015 where

import AoCHelper (Pair, iter, splitBy, (.+.), both)
import Control.Applicative (many, (<|>))
import Data.Array (indices)
import Data.Foldable (asum)
import Data.List as List (elemIndex, group, isInfixOf, isSubsequenceOf, sort, transpose, (\\), permutations, groupBy, sortOn, scanl', foldl', foldr, subsequences, isPrefixOf, tails, inits, findIndex)
import Data.Map as Map (Map, fromList, findWithDefault)
import Data.Maybe (fromJust)
import Data.Ord (Down(Down))
import Data.Set as Set (Set, fromList, toList, size, union)
import Data.Text (pack, unpack, replace)
import Data.Text.Internal.Search as Search (indices)
import MPCAS (Parser, alphanum, anyChar, char, ident, integer, natural, runParser, symbol, upper)

{-- day 1 --}

day1 :: String -> [Int]
day1 = map toInt
  where
    toInt '(' = 1
    toInt ')' = -1
    toInt _ = 0

{-- try part 2 with parsing
nested :: Parser Maybe Int
nested = (+) <$> parenthezised nested <*> nested <|> pure 2
day1_2 input = (-1) + (maybe 0 fst . runParser nested $ input)
 --}

day1_1, day1_2, day1_3 :: String -> Int
day1_1 = sum . day1
day1_2 = length . takeWhile (>= 0) . scanl (+) 0 . day1
day1_3 = maximum . scanl (+) 0 . day1

-- 138
-- 1771
-- 160

{-- day 2 --}

day2 :: ([Int] -> Int) -> [String] -> Int
day2 toInt = sum . map (toInt . map read . splitBy (=='x'))

day2_1, day2_2 :: [String] -> Int
day2_1 = day2 toInt
  where
    toInt [a, b, c] =
      let [x, y, z] = [a * b, a * c, b * c]
       in 2 * (x + y + z) + minimum [x, y, z]
day2_2 = day2 toInt
  where
    toInt [a, b, c] =
      let [x, y, z] = sort [a, b, c]
       in x * y * z + 2 * (x + y)

-- 1606483
-- 3842356

{-- day 3 --}

toDir :: Char -> (Int, Int)
toDir '<' = (-1, 0)
toDir '>' = (1, 0)
toDir '^' = (0, 1)
toDir 'v' = (0, -1)
toDir  _  = (0, 0)

evenOdds :: [a] -> ([a], [a])
evenOdds (x : y : xs) =
  let (as, bs) = evenOdds xs
   in (x : as, y : bs)
evenOdds xs = (xs, [])

day3 :: String -> Set (Int, Int)
day3 = Set.fromList . scanl (.+.) (0, 0) . map toDir

day3_1, day3_2 :: String -> Int
day3_1 = size . day3
day3_2 input = size $ day3 s `union` day3 r
  where
    (s, r) = evenOdds input

-- 2081
-- 2341

{-- day 4 --}

-- implemented in Squeak Smalltalk, cause using md5 is simpler there

-- 117946
-- 3938038

{-- day 5 --}

hasDoubleDouble :: String -> Bool
hasDoubleDouble xs@(x : y : rest) = List.isInfixOf (x : [y]) rest || hasDoubleDouble (tail xs)
hasDoubleDouble _ = False

hasTripplePalindrom :: String -> Bool
hasTripplePalindrom xs@(x : y : z : rest) = x == z || hasTripplePalindrom (tail xs)
hasTripplePalindrom _ = False

day5_1, day5_2 :: [String] -> Int
day5_1 =
  length
    . filter ((> 2) . length . filter (`elem` "aeiou"))
    . filter (any ((> 1) . length) . group)
    . filter (\xs -> not (any (($ xs) . List.isInfixOf) ["ab", "cd", "pq", "xy"]))
day5_2 =
  length
    . filter hasTripplePalindrom
    . filter hasDoubleDouble

-- 258
-- 53

{-- day 6 --}

-- extra module Day6.hs

-- 543903

{-- day 7 --}

-- extra module Day7.hs

-- 16076
-- 2797

{-- day 8 --}

countChar :: String -> Int
countChar [] = 0
countChar ('\\' : 'x' : rest) = (+ 1) . countChar . drop 2 $ rest
countChar ('\\' : rest) = (+ 1) . countChar . tail $ rest
countChar ('\"' : rest) = countChar rest
countChar xs = (+ 1) . countChar . tail $ xs

day8_1, day8_2 :: [String] -> Int
day8_1 input = sumOfChar - sumOfAsciiChar
  where
    sumOfChar = sum . map length $ input
    sumOfAsciiChar = sum . map countChar $ input
day8_2 = sum . map ((+ 2) . length . filter (`elem` "\\\""))

-- 1342
-- 2074

{-- day 9 --}

-- calculated by hand!

-- 209 Tristram -> Tambi -> Snowdin -> AlphaCentauri -> Faerun -> Arbre -> Straylight -> Norrath
--              49       15         12               13        24       40            54  
-- 804 Tambi -> Faerun -> Norrath -> Tristram -> AlphaCentauri -> Arbre -> Snowdin -> Straylight
--           71        129        142         118              116      129        99

{-- day 10 --}

step :: String -> String
step xs = concat . zipWith (++) (map (show . length) grp) $ map ((: "") . head) grp
  where
    grp = group xs

day10 :: Int -> Int
day10 n = length . iter n step $ "1113122113"

day10_1, day10_2 :: Int
day10_1 = day10 40
day10_2 = day10 50

-- 360154
-- 5103798

{-- day 11 --}

input :: String
input = "vzbxkghb"

{-- having a good look at the input and next strings: --}

-- vzbxxyzz
-- vzcaabcc

{-- day 12 --}

specialChars :: [Char]
specialChars = ['[', ']', '{', '}', ':', ',', '"']

extractOthers :: Parser Maybe [String]
extractOthers = many (asum (map (symbol . (: "")) specialChars) <|> ident)

extractNumbers :: Parser Maybe [Int]
extractNumbers = many (extractOthers *> integer)

day12_1 :: String -> Int
day12_1 = sum . maybe [] fst . runParser extractNumbers

-- parsed part 2 by hand!

-- 156366
-- 96852

{-- day 13 --}

happy :: Parser Maybe (Pair String, Int)
happy = let uIdent = (:) <$> upper <*> many alphanum in do
  first <- uIdent
  symbol "would"
  pm <- ident
  n <- integer
  symbol "happiness units by sitting next to"
  second <- uIdent
  char '.'
  return ((first, second), if pm == "gain" then n else -n)

parseHappy :: String -> (Pair String, Int)
parseHappy = maybe (("", ""), 0) fst . runParser happy

totalHappiness :: Map.Map (Pair String) Int -> [String] -> Int
totalHappiness happyMap seating = sum . zipWith happiness seating $ last seating : init seating
  where
    happiness x y = ((+) <$> Map.findWithDefault 0 (x, y) <*> Map.findWithDefault 0 (y, x)) happyMap

day13 :: [String] -> Pair Int
day13 input = both f (people, "TT" : people)
  where
    f = maximum . map (totalHappiness happyMap) . permutations
    lst = map parseHappy input
    happyMap = Map.fromList lst
    people = Set.toList . Set.fromList $ concat [[x, y] | ((x, y), _) <- lst]

-- 618
-- 601


{-- day 14 --}

reindeerParser :: Parser Maybe (Int, Int, Int)
reindeerParser = do
  many alphanum
  symbol "can fly"
  v <- natural
  symbol "km/s for"
  d <- natural
  symbol "seconds, but then must rest for"
  p <- natural
  symbol "seconds."
  return (v, d, p)

parseReindeer :: String -> (Int, Int, Int)
parseReindeer = maybe (0, 0, 0) fst . runParser reindeerParser

time :: Int
time = 2503

day14_1, day14_2 :: [String] -> Int
day14_1 = maximum . map (calc . parseReindeer)
  where
    calc (v, d, p) = let i = d + p in v * (d * div time i + min d (time `mod` i))

day14_2 = maximum . foldr (zipWith (+) . g) (repeat 0) . transpose . map (f . parseReindeer)
  where
    f (v, d, p) = take time . tail . scanl (+) 0 . cycle $ replicate d v ++ replicate p 0
    g xs = let m = maximum xs in map (fromEnum . (== m)) xs

-- 2660
-- 1256


{-- day 15 --}

extractIngredient :: String -> Parser Maybe Int
extractIngredient i = anyChar *> symbol i *> integer

extractIngredients :: Parser Maybe (Int, Int, Int, Int, Int)
extractIngredients = do
  many alphanum
  c <- extractIngredient "capacity"
  d <- extractIngredient "durability"
  f <- extractIngredient "flavor"
  t <- extractIngredient "texture"
  k <- extractIngredient "calories"
  return (c, d, f, t, k)

parseIngredients :: String -> (Int, Int, Int, Int, Int)
parseIngredients = maybe (0, 0, 0, 0, 0) fst . runParser extractIngredients

r :: (Int, Int, Int, Int, Int) -> Int
r (x1, x2, x3, x4, x5) = max 0 x1 * max 0 x2 * max 0 x3 * max 0 x4

day15 :: [String] -> [(Int, Int, Int, Int, Int)]
day15 input =
  [ f x1 x2 x3 x4
    | x1 <- [0 .. 100],
      x2 <- [0 .. 100 - x1],
      x3 <- [0 .. 100 - x1 - x2],
      x4 <- [0 .. 100 - x1 - x2 - x3],
      x1 + x2 + x3 + x4 == 100
  ]
  where
    mixtures = map parseIngredients input
    f x1 x2 x3 x4 = foldr h (0, 0, 0, 0, 0) $ zipWith g [x1, x2, x3, x4] mixtures
    g x (c, d, f, t, k) = (c * x, d * x, f * x, t * x, k * x)
    h (x1, x2, x3, x4, x5) (y1, y2, y3, y4, y5) = (x1 + y1, x2 + y2, x3 + y3, x4 + y4, x5 + y5)

day15_1, day15_2 :: [String] -> Int
day15_1 = maximum . map r . day15
day15_2 = maximum . map r . filter (\(c, d, f, t, k) -> k == 500) . day15

-- 13882464
-- 11171160

{-- day 16 --}

-- this little problem could be done by hand!

day16_1, day16_2 :: [String] -> Maybe Int
day16_1 input = List.elemIndex 3 filterProperties
  where
    filterProperties = map (length . filter id . f) input
    f xs = map (($ xs) . List.isInfixOf) properties
day16_2 = const $ Just 260

properties :: [String]
properties =
  [ "children: 3",
    "cats: 7", -- >
    "samoyeds: 2",
    "pomeranians: 3", -- <
    "akitas: 0",
    "vizslas: 0",
    "goldfish: 5", -- <
    "trees: 3", -- >
    "cars: 2",
    "perfumes: 1"
  ]

-- 373
-- 260

{-- day 17 --}

count :: Int -> [Int] -> Int
count value containers
  | value == 0 = 1
  | null possibile = 0
  | otherwise = count value rest + count (value - head possibile) rest
  where
    possibile = filter (<= value) containers
    rest = tail possibile

day17_1 :: [String] -> Int
day17_1 = count 150 . reverse . sort . map read

-- ToDo: make it right!
day17_2 :: [String] -> [(Int, Int, Int, Int)]
day17_2 xs =
  sort [(a, b, c, d) |
  a <- containers,
  b <- containers,
  a >= b,
  c <- containers,
  b >= c,
  d <- containers,
  c >= d,
  a + b + c + d == 150,
  [a, b, c, d] `isSubsequenceOf` containers]
  where
    containers = reverse . sort . map read $ xs

-- 1638
-- 17

{-- day 18 --}

-- extra module Day18.hs

-- 1061
-- 1006


{-- day 19 --}

day19_1 :: [String] -> Int
day19_1 input = size . Set.fromList . concat $ zipWith g substitutions indexLst
  where
    f (old, new) idx =
      let (l, r) = splitAt idx string
       in l ++ new ++ drop (length old) r
    g substitution = map (f substitution)
    indexLst = map (flip Search.indices (pack string) . pack . fst) substitutions
    substitutions = map (((,) <$> head <*> last) . words) sub
    string = head str
    [sub, str] = splitBy (== "") input

day19_2 :: [String] -> Int
day19_2 input = succ . sum . map snd . takeWhile ((/= pack "e") . fst) $ iterate (g . fst) (pack string, 0)
  where
    g s = f (indexLst (unpack s) substitutions) (unpack s)
    f il s = let ((from, to), idxs) = head $ dropWhile (null . snd) il in (replace (pack from) (pack to) (pack s), length idxs) 
    indexLst s = zip <*> map (flip Search.indices (pack s) . pack . fst)
    substitutions = sortOn (Data.Ord.Down . length . fst) $ map (((,) <$> last <*> head) . words) sub
    string = head str
    [sub, str] = splitBy (== "") input

-- 535
-- 212


{-- day 20 --}

-- input: 34000000

-- implemented in Squeak Smalltalk

-- 786240
-- 831600  oracled by copilot!-O 


{-- day 23 --}

{-- looking at the input code:
    first two parts are calculating some integers
    part three is calculating the collatz sequence of this values
    b is the length of this sequence!
--}

collatz :: Integer -> [Integer]
collatz = takeWhile (> 1) . iterate c
  where
    c n
      | odd n = 3 * n + 1
      | otherwise = div n 2

day23 :: Integer -> Int
day23 = length . collatz

day23_1, day23_2 :: Int
day23_1 = day23 26623
day23_2 = day23 31911

-- 307
-- 160

{-- day 24 --}

day24_1, day24_2 :: [String] -> Integer
day24_1 input = minimum . map product . filter ((<= 6) . length) . filter ((== q) . sum) $ subsequences ns
  where q = sum ns `div` 3
        ns = map read input

day24_2 input = minimum . map product . filter ((<= 5) . length) . filter ((== q) . sum) $ subsequences ns
  where q = sum ns `div` 4
        ns = map read input

-- 10439961859
-- 72050269

-- both solutions have a duration of about 2 minutes!


{-- day 25 --}

{-- To continue, please consult the code grid in the manual.
    Enter the code at row 2947, column 3029. --}

-- 1/1                                -> 2            1
-- 2/1, 1/2                           -> 3            2
-- 3/1, 2/2, 1/3                      -> 4            3
-- ...                                              ...
-- ...                                             5974
-- 5975/1, 5874/2, ..., 2947/3029, ... -> 5976     5975

-- So, locking for the iteration
-- 1 + 2 + 3 + ... + 5974 + 3029 = 5974 * 5975 / 2 + 3029 = 17850272
-- of f x =  mod (x * 252533) 33554393 solves this problem.
-- But iter and iterate thrown both an stack overflow!-(

-- implemented in Squeak Smalltalk

-- 19980801

main = do
  input <- readFile "19_2015.txt"
  print . day19_1 $ lines input
  print . day19_2 $ lines input