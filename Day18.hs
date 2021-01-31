module Day18 where

import MPCAS (Parser, runParser, symbol, natural)
import Control.Applicative (Alternative ((<|>), many))

{-- Parsing with normal precedence --}

expr :: Parser Int
expr = do 
  t <- term
  do symbol "+"
     e <- expr
     return (t + e)
   <|> return t
           
term :: Parser Int
term = do 
  f <- subExpr expr
  do symbol "*"
     t <- term
     return (f * t)
   <|> return f

{-- Parsing with inverted precedence --}

expr' :: Parser Int
expr' = do
  t <- term'
  do symbol "*"
     e <- expr'
     return (t * e)
   <|> return t
           
term' :: Parser Int
term' = do 
  f <- subExpr expr'
  do symbol "+"
     t <- term'
     return (f + t)
   <|> return f

{-- Parsing without priority --}

opLst = [("+", (+)), ("*", (*))]

operations :: Parser (Int -> Int)
operations = do 
  o <- symbol "+" <|> symbol "*"
  r <- subExpr exprL2R
  let Just op = lookup o opLst in
    return (op r)   

exprL2R :: Parser Int
exprL2R = do 
  l <- subExpr exprL2R
  ops <- many operations
  return (foldr ($) l (reverse ops))

{-- subExpr works for all parsers --}
           
subExpr :: Parser Int -> Parser Int
subExpr expr = natural <|> symbol "(" *> expr <* symbol ")" 

{-- Evaluator --}

eval :: Parser Int -> String -> Int
eval parser xs = case runParser parser xs of
  Just (n,[])  -> n
  Just (_,out) -> error ("Unused input " ++ out)
  Nothing      -> error "Invalid input"

day18_0 = sum . map (eval expr)       -- Normal calculation! Not part of the puzzle.
day18_1 = sum . map (eval exprL2R)
day18_2 = sum . map (eval expr')

-- 490511034040
-- 21022630974613
-- 169899524778212


main = do
  input <- readFile "day18.input"
  print . day18_0 . lines $ input
  print . day18_1 . lines $ input
  print . day18_2 . lines $ input