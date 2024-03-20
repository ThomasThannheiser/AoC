module Day19 where

import AoCHelper (splitBy, splitWith)
import Data.Map (Map, adjust, fromList, unionWith, (!))
import Data.Map qualified as Map (foldr, map)

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Read)

data Sampling = A | R deriving (Eq)

type Rule = Part -> Sampling

type Name = String

type Description = String

accepted, rejected :: Rule
accepted = const A
rejected = const R

type Category = Part -> Int

char2cat :: Char -> Category
char2cat 'x' = x
char2cat 'm' = m
char2cat 'a' = a
char2cat 's' = s

type Predicate = Int -> Bool

char2comp :: Char -> Int -> Predicate
char2comp '<' n = (< n)
char2comp '>' n = (> n)

day19 :: [String] -> ([Part], Map String String)
day19 input = (parts, rules)
  where
    parts = map (read . ("Part" ++)) $ snd input'
    rules = fromList . addSampling . map (splitWith '{' . init) $ fst input'
    addSampling = (("A", "A") :) . (("R", "R") :)
    input' = parse input
    parse = ((,) <$> head <*> last) . splitBy (== "")

make :: Description -> Map Name Description -> (Char -> Predicate -> a -> a -> a) -> a -> a -> a
make xs m make' acc rej
  | xs == "A" = acc
  | xs == "R" = rej
  | otherwise = case parseRule xs of
      Left name ->
        make (m ! name) m make' acc rej
      Right (xmas, predicate, name, descr) ->
        make' xmas predicate (make (m ! name) m make' acc rej) (make descr m make' acc rej)

parseRule :: String -> Either Name (Char, Predicate, Name, Description)
parseRule xs
  | ',' `elem` xs -- x<345:asdf,jk
    =
      let (f, descr) = splitWith ',' xs
          (cond, nextRule) = splitWith ':' f
          xmas = head cond
          lg = cond !! 1
          n = drop 2 cond
       in Right (xmas, char2comp lg (read n), nextRule, descr)
  | otherwise = Left xs

day19_1 :: [String] -> Int
day19_1 input = sum . map value $ filter ((== A) . makeRule (rules ! "in") rules) parts
  where
    (parts, rules) = day19 input
    value (Part x m a s) = x + m + a + s

makeRule :: String -> Map String String -> Rule
makeRule xs m = make xs m makeRule' accepted rejected
  where makeRule' cat comp g h part = (if comp $ char2cat cat part then g else h) part

type Filters = Map Char [[Int] -> [Int]]

acceptAll, rejectAll :: Filters
acceptAll = fromList $ map (, [id]) "xmas"
rejectAll = fromList $ map (, []) "xmas" 

day19_2 :: [String] -> Int
day19_2 input = sum . Map.foldr (zipWith (*)) (repeat 1) $ Map.map (map (length . ($ [1..4000]))) filters 
  where
    filters = makeFilters (rules ! "in") rules
    (_, rules) = day19 input

makeFilters :: Description -> Map Name Description -> Filters
makeFilters xs m = make xs m makeFilters' acceptAll rejectAll
  where  makeFilters' cat predicate l r = 
          let lf = g predicate l
              rf = g (not . predicate) r
              g p = adjust (map (filter p .)) cat
           in unionWith (++) lf rf

-- 509597
-- 143219569011526

main :: IO ()
main = do
  input <- readFile "day19.input"
  print . day19_1 . lines $ input
  print . day19_2 . lines $ input