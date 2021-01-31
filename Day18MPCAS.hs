module Day18MPCAS where

import MPCAS (Parser, runParser, symbol, natural, chainl1, parenthezised)

import Data.Functor (($>))
import Control.Applicative (Alternative ((<|>)))

{-- simple expression parsers --}

expr, expr', expr'' :: Parser Int
-- normal precedence (* before +)
expr = buildExpr expr [mulop, addop]
-- inverted precedence (+ befor *)
expr' = buildExpr expr' [addop, mulop]
-- no precedence
expr'' = buildExpr expr'' [addop <|> mulop]

buildExpr :: Parser Int -> [Parser (Int -> Int -> Int)] -> Parser Int
buildExpr expr = foldl chainl1 (factor expr)

factor :: Parser Int -> Parser Int
factor expr = natural <|> parenthezised expr

addop, mulop :: Parser (Int -> Int -> Int)
addop = symbol "+" $> (+) <|> symbol "-" $> (-)
mulop = symbol "*" $> (*) <|> symbol "/" $> div

{-- some examples --}

main = do
    -- print $ runParser expr "(1 + 2 * 4) / 3 + 5"
    -- print $ runParser expr "1-2-3-4"
    -- print $ runParser expr "12 - 13"
    input <- readFile "day18.input"
    mapM_ (print . ($ input) . calc) [expr, expr'', expr']
    --  490511034040, 21022630974613, 169899524778212
    where calc parser = sum . map (maybe 0 fst . runParser parser) . lines