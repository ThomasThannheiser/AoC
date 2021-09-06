module Day21 where

import MPCAS (Parser, runParser, char, identifier, string, sepBy1, parenthezised)
import Data.List (intercalate, sortBy, (\\), intersect)
import Data.Set (fromList, toList)
import Control.Applicative (Alternative (many, some))

{-- parsing stuff --}

allergens :: Parser Maybe [String]
allergens = parenthezised $ string "contains" *> identifier `sepBy1` char ','

extractFood :: Parser Maybe ([String], [String])
extractFood = (,) <$> some identifier <*> allergens
            
parseFood :: String -> ([String], [String])
parseFood = maybe ([], []) fst . runParser extractFood

 --}

intersectIngrediences :: [([String], [String])] -> [String] -> String -> [String]
intersectIngrediences input ingredLst allergen =
  foldr (intersect . fst) ingredLst . filter (elem allergen . snd) $ input

countIngredience :: String -> [([String], [String])] -> Int
countIngredience ingredience = length . filter (elem ingredience . fst)

splitAndDrop :: Int -> [(String, [String])] -> [(String, [String])]
splitAndDrop n sorted = allergen ++ map (\(x, y) -> (x, y \\ snd (last allergen))) rest
  where (allergen, rest) = splitAt n sorted

sortByLength, repeatedSortAndDrop :: [(String, [String])] -> [(String, [String])]
sortByLength = sortBy (\(_, x) (_, y) -> compare (length x) (length y))

repeatedSortAndDrop = foldr (.) id . reverse . map (\n -> splitAndDrop n . sortByLength) $ [1..7]

concatToSet :: (([String], [String]) -> [String]) -> [([String], [String])] -> [String]
concatToSet part = toList . fromList . concatMap part

day21 :: [String] -> ([String], [String], [[String]])
day21 lst = (ingrediences, allergens, intersections)
  where input = map parseFood lst
        ingrediences = concatToSet fst input
        allergens = concatToSet snd input
        intersections = map (intersectIngrediences input ingrediences) allergens

day21_1 lst = sum . map (`countIngredience` input) $ noAllergens
  where noAllergens = ingrediences \\ concat intersections
        input = map parseFood lst
        (ingrediences, _, intersections) = day21 lst
        
day21_2 lst = intercalate "," . concatMap snd $ orderedAllergens
  where orderedAllergens = sortBy (\(x, _) (y, _) -> compare x y) . repeatedSortAndDrop $ zip allergens intersections
        (_, allergens, intersections) = day21 lst

-- 2584
-- fqhpsl,zxncg,clzpsl,zbbnj,jkgbvlxh,dzqc,ppj,glzb


main = do
  input <- readFile "day21.input"
  print . day21_1 . lines $ input
  print . day21_2 . lines $ input