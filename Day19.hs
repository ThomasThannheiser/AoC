module Day19 where

import AoCHelper (split')
-- import Data.List (inits, tails) 

{-- parsing stuff --}

data Expr = Val Char 
          | And Expr Expr 
          | Or Expr Expr 
          | See Int
          deriving Show

value :: Expr -> [String]
value (Val c) = [[c]]
value (And a b) = [x ++ y | x <- value a, y <- value b]
value (Or x y) = value x ++ value y

{-- try to evaluate the strings against Expr
    takes too much time for longer strings!-(
accept :: Expr -> String -> Bool
accept (Val c) s   = s == [c]
accept (Or a b) s  = accept a s || accept b s
accept (And a b) s = or [accept a u && accept b v | (u, v) <- split'' s]

split'' :: [a] -> [([a], [a])]
split'' xs = zip (inits xs) (tails xs) 
--}

{-- generating Regex 
expr2Regex :: Expr -> String
expr2Regex (Val c)   = c 
expr2Regex (And x y) = expr2Regex x ++ expr2Regex y
expr2Regex (Or x y)  = "(" ++ expr2Regex x ++ "|" ++ expr2Regex y ++ ")"

 --}

parse :: [(Int, [[String]])] -> Int -> Expr
parse rules n  = case lookup n rules of
  Just xs -> parse' rules xs 

parse' :: [(Int, [[String]])] -> [[String]] -> Expr
parse' rules [["\"a\""]] = Val 'a' 
parse' rules [["\"b\""]] = Val 'b'
parse' rules [[x]] = parse rules (read x)
parse' rules [x : xs] = And (parse rules (read x)) (parse' rules  [xs])
parse' rules [xs, ys] = Or (parse' rules [xs]) (parse' rules [ys])
  
parseRule :: String -> (Int, [[String]])
parseRule s = (read nr, rs)
  where rs = map words . split' (== '|') $ r
        (nr, ':' : ' ' : r) = span (/= ':') s
 
 --}
 
dropFstLst :: String -> String
dropFstLst = reverse . drop 8 . reverse . drop 8

has4231 :: [String] -> [String] -> String -> Bool
has4231 _42 _31 s = has42 _42 s && has31 _31 s

has31 :: [String] -> String -> Bool
has31 _31 s = reverse (take 8 (reverse s)) `elem` _31

has42 :: [String] -> String -> Bool
has42 _42 s = take 8 s `elem` _42

dropWhile4231 :: [String] -> [String] -> String -> String
dropWhile4231 _42 _31 s = if has4231 _42 _31 s then dropWhile4231 _42 _31 (dropFstLst s) else s

dropWhile42 :: [String] -> String -> String
dropWhile42 _42 s = if has42 _42 s then dropWhile42 _42 (drop 8 s) else s

-- Part 1 takes over a minute!-(
-- self parsing "accept" dosn't work in acceptable time for long strings
-- ToDo: take regEx!? 
-- Regex dosn't work in Haskell!-O
-- Solution with simple parser combinators in "Day19 Parser.hs"!-)
day19_1 input = parse r 0 
  --length $ filter (`elem` value (parse r 0)) messages
  where r = map parseRule rules
        (rules, "" : messages) = break null input
        
day19_2 input = length . filter null . filter4231 $ messages
  where filter4231 = map (dropWhile42 _42) . filter (has42 _42) . map (dropWhile4231 _42 _31) . filter (has4231 _42 _31)
        _31 = value $ parse r 31
        _42 = value $ parse r 42
        r = map parseRule rules
        (rules, "" : messages) = break null input
        -- 19_1: length $ filter (`elem` lex) messages
        -- lex = [x++y++z | x <- _42, y <- _42, z <- _31]

-- 176
-- 352        

        
main = do
  input <- readFile "day19.input"
  print . day19_1 . lines $ input
  print . day19_2 . lines $ input