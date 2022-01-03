module Day14 where

import MPCAS (Parser, runParser, char, symbol, upper)
import AoCHelper (iter, split')
import Data.List (group, groupBy, sort)
import Control.Applicative (Alternative (many))
import Data.Bifunctor (first)

type Rule = (String, Char)

extractRule :: Parser Maybe Rule
extractRule = do
  left <- many upper 
  symbol "->"
  c <- upper
  return (left, c)

parseRule :: String -> Rule
parseRule = maybe ("", '-') fst . runParser extractRule

str2pairs :: String -> [String]
str2pairs xs = zipWith (\x y -> x : y : "") xs (tail xs)

f :: [Rule] -> String -> String
f rules x = head x : c : ""
  where Just c = lookup x rules

replace' :: [Rule] -> String -> String
replace' rules str = concatMap (f rules) pairs ++ [last str]
  where pairs = str2pairs str  
 
day14_1:: [String] -> Int
day14_1 input = maximum counts - minimum counts
  where 
    counts =  map length . group . sort $ afterIteration 
    afterIteration = iter (replace' rules) 10 polymer
    polymer = head p
    rules = map parseRule r
    [p, r] = split' null input

insertion :: [Rule] -> ([Char], b) -> [([Char], b)]
insertion rules (x, n) = [(head x : c : "", n), (c : last x : "", n)] 
  where Just c = lookup x rules 

sumUp :: Eq a => [(a, Int)] -> [(a, Int)]
sumUp = map (\grp -> (fst . head $ grp, sum . map snd $ grp)) . groupBy (\x y -> fst x == fst y)

replace'' :: [Rule] -> [(String, Int)] -> [(String, Int)]
replace'' rules = sumUp . sort . concatMap (insertion rules) 

day14 :: Int -> [String] -> Int
day14 iterations input = maximum counts - minimum counts
  where 
    counts = map snd . sumUp . ((firstChar, 1) : ) . sort . map (first last) $ afterIteration 
    afterIteration = iter (replace'' rules) iterations pairs
    pairs = map (\xs -> (head xs, length xs)) . group . sort . str2pairs $ polymer
    firstChar = head polymer
    polymer = head p
    rules = map parseRule r
    [p, r] = split' null input

-- 2703
-- 2984946368465


main = do
  print . day14_1 . lines =<< readFile "day14.input"
  print . day14 40 . lines =<< readFile "day14.input"