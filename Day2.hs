module Day2 where

import AoCHelper (between, split')
-- import MPCAS (Parser, runParser, natural, symbol, lower)
-- import Control.Applicative (Alternative (some))

{-- parsing with simple parser combinators --

extract :: Parser Maybe ((Int, Int), Char, String)
extract = do
  n <- natural
  symbol "-"
  m <- natural
  c <- lower
  symbol ":"
  s <- some lower
  return ((n, m), c, s)

parse :: String -> ((Int, Int), Char, String)
parse = maybe ((0,0), '', "") fst . runParser extract

 --}

{-- parsing by hand --}

parse :: String -> ((Int, Int), Char, String)
parse input = ((n, m), c, pw)
  where (n, m) = (read a, read b)
        [a, b] = split' (== '-') numbers
        [numbers, c : ":", pw] = words input
 --}

validPW1, validPW2 :: ((Int, Int), Char, String) -> Bool
validPW1 ((min, max), c, code) = count `between` (min, max)
  where count = length . filter (== c) $ code

validPW2 ((pos1, pos2), c, code) = codeAtPosOk pos1 /= codeAtPosOk pos2
  where codeAtPosOk pos = code !! (pos - 1) == c

day2 validCond = length . filter validCond . map parse
day2_1 = day2 validPW1
day2_2 = day2 validPW2

-- 614
-- 354


main = do
  input <- readFile "day2.input"
  print . day2_1 . lines $ input
  print . day2_2 . lines $ input