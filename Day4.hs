module Day4 where

import AoCHelper (between, split')
import Data.List (lookup, isInfixOf)
import Data.Char (isDigit, isHexDigit)

passports :: [String] -> [[String]]
passports = map (concatMap words) . split' null

filterLength :: Int -> [String] -> [[String]]
filterLength n = filter ((== n) . length) . passports

valid :: [String] -> [[String]]
valid lst = filterLength 8 lst ++ (map words . valid7 $ lst)

valid7 :: [String] -> [String]
valid7 = filter (not . isInfixOf "cid") . map unwords . filterLength 7

toPair :: String -> (String, String)
toPair str = (a, b)
  where (a, _ : b) = span (/= ':') str

validDict :: [String] -> [[(String, String)]]
validDict = map (map toPair) . valid

yearCondition :: (Int, Int) -> String -> Bool
yearCondition (min, max) value = read value `between` (min, max)

eclCondition, pidCondition, hclCondition, hgtCondition :: String -> Bool
eclCondition = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
pidCondition value = length value == 9 && all isDigit value
hclCondition (x : xs) = x == '#' && all isHexDigit xs
hgtCondition value = let unit = reverse . take 2 . reverse $ value
                         wert = read . init . init $ value in
  unit == "cm" && (wert `between` (150, 193)) ||
  unit == "in" && (wert `between` ( 59,  76))

filter' :: String -> (String -> Bool) -> [[(String, String)]] -> [[(String, String)]]
filter' key condition = filter $ evalValueCondition key condition

evalValueCondition :: String -> (String -> Bool) -> [(String, String)] -> Bool
evalValueCondition key condition dict = condition $ getValue key dict

getValue :: String -> [(String, String)] -> String
getValue key dict = value
  where Just value = lookup key dict

day4_1 = length . valid
day4_2 = length .
    filter' "pid" pidCondition . 
    filter' "ecl" eclCondition . 
    filter' "hcl" hclCondition . 
    filter' "hgt" hgtCondition .
    filter' "eyr" (yearCondition (2020, 2030)) . 
    filter' "iyr" (yearCondition (2010, 2020)) . 
    filter' "byr" (yearCondition (1920, 2002)) .
    validDict

-- 219
-- 127


main = do
  input <- readFile "day4.input"
  print . day4_1 . lines $ input
  print . day4_2 . lines $ input