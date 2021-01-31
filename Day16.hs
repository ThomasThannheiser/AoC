module Day16 where

import AoCHelper (between, split', readIntLst)
import Data.List (sortBy, intersect, (\\))
import Data.Char (isDigit)

toInts :: String -> [Int]
toInts = map read . split' (== '-')

nr x = (toInts . head $ x, toInts . last $ x)

validS n rule = between n (head . fst $ rule, last . fst $ rule) ||
                between n (head . snd $ rule, last . snd $ rule)
validM rules n = any (validS n) rules
allValid rules = all (validM rules)

rulesFor rules n = filter (validS n) rules

rx ruleNrs validNbs n = map (rulesFor ruleNrs . (!!n)) validNbs

validRulesforAllAtPos ruleNrs validNbs n = foldr1 intersect $ rx ruleNrs validNbs n

parse16 :: [String] -> ([String], [Int], [[Int]])
parse16 lst = (rules, myTicketNrs, nearByNrs)
  where nearByNrs = map readIntLst nearbys
        myTicketNrs = readIntLst myTicket
        [rules, "your ticket:" : [myTicket], "nearby tickets:" : nearbys] = split' null lst

day16_1 lst = sum . concatMap (filter (not . validM ruleNrs)) $ nbNrs
  where ruleNrs = map (nr . words . dropWhile (not . isDigit)) rules
        (rules, _, nbNrs) = parse16 lst
        
day16_2 lst = product . map ((myTicketNrs!!) . fst) $ departureNrs
  where departureNrs = filter (\(_, y : ys) -> y `elem` departure) . sortBy (\(x, _) (y, _) -> compare x y) $ unsortedRules
        unsortedRules = last sortedRules : zipWith (\(x1, x2) (_, y) -> (x1, x2 \\ y)) sortedRules (tail sortedRules)
        sortedRules = sortBy (\(_, x) (_, y) -> compare (length y) (length x)) $ zip [0..19] validFor
        validFor = map (validRulesforAllAtPos ruleNrs validNbs) [0..19]
        validNbs = filter (allValid ruleNrs) nbNrs
        ruleNrs = map (nr . words . dropWhile (not . isDigit)) rules
        (rules, myTicketNrs, nbNrs) = parse16 lst
        departure = take 6 ruleNrs

-- 22000
-- 410460648673


main = do
  input <- readFile "day16.input"
  print . day16_1 . lines $ input
  print . day16_2 . lines $ input