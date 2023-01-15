module Day21 where

import AoCHelper (splitWith)

import Data.Map as Map (Map, fromList, alter, delete, findWithDefault)
import Data.Bifunctor (second)
import Data.Ratio ((%))

str2Op :: String -> (Rational -> Rational -> Rational)
str2Op "+" = (+)
str2Op "-" = (-)
str2Op "*" = (*)
str2Op "/" = (/)
str2Op  _  = const

eval :: String -> Map String [String] -> Rational
eval identifier dict = case findWithDefault [] identifier dict of
                        [x]       -> read x % 1
                        [x, y, z] -> str2Op y (eval x dict) (eval z dict)
                        _         -> 0
             
day21 :: [String] -> Map String [String]
day21 = fromList . map (second words . splitWith ':')

day21_1:: [String] -> Rational
day21_1 = eval "root" . day21 

day21_2 :: [String] -> Rational
day21_2 input = zero / (zero - comp 1) 
  where 
    zero = comp 0
    comp v = eval "root" 
             . alter (\(Just [x, _, z]) -> Just [x, "-", z]) "root" 
             . alter (\(Just [_]) -> Just [show v]) "humn" . day21 $ input
        
-- 194501589693264
-- 3887609741189

main :: IO ()
main = do
  input <- readFile "day21.input"
  print . day21_1 . lines $ input
  print . day21_2 . lines $ input