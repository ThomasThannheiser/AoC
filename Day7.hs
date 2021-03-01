module Day7 where

import MPCAS (Parser, runParser, char, space, lower, integer, string, identifier, sepBy)
import Data.List ((\\))
import Data.Set as Set (fromList, toList)
import Data.Functor (($>))
import Data.Bifunctor (second)
import Control.Applicative (Alternative ((<|>), many, some))

{-- parsing stuff --}

color :: Parser Maybe String
color = twoWords <$> identifier <*> identifier
  where twoWords xs ys = xs ++ (' ' : ys) 
  
outerBag :: Parser Maybe String
outerBag = color <* string "bags contain "
  
noInnerBags :: Parser Maybe [(Int, String)]
noInnerBags = string "no other bags" $> []  

innerBags :: Parser Maybe [(Int, String)]
innerBags = (( , ) <$> integer <*> color <* some lower) `sepBy` char ','
  
parseLine :: Parser Maybe (String, [(Int, String)])
parseLine = ( , ) <$> outerBag <*> (noInnerBags <|> innerBags) <* char '.'
  
rules :: [String] -> [(String, [(Int, String)])]
rules = map (maybe ("", []) fst . runParser parseLine)

 --}

collectFor :: [(String, [String])] -> String -> [String]
collectFor lst color = map fst . filter (\(_, y) -> color `elem` y) $ lst

collect :: [(String, [String])] -> [String] -> [String]
collect lst colors = toList . fromList $ colors ++ concatMap (collectFor lst) colors

stopIterate :: [[String]] -> [String]
stopIterate (x : (y : xs))
  | null (y \\ x) = x
  | otherwise = stopIterate (y : xs)

colorCount :: [(String, [(Int, String)])] -> String -> Int
colorCount lst color = if null colorCounts then 1
  else 1 + sum (zipWith (*) (map fst colorCounts) (map (colorCount lst . snd) colorCounts))
  where Just colorCounts = lookup color lst

shinyGold = "shiny gold"

day7_1 lst = length (stopIterate sets) - 1 
  where sets = iterate (collect input) [shinyGold]
        input = map (second (map snd)) (rules lst)
day7_2 lst = pred . colorCount (rules lst) $ shinyGold

-- 121
-- 3805


main = do 
  input <- readFile "day7.input"
  print . day7_1 . lines $ input
  print . day7_2 . lines $ input