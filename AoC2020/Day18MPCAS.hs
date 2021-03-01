module Day18MPCAS where

import MPCAS (Parser, runParser, symbol, natural, chainl1, parenthezised)
import Control.Applicative (Alternative ((<|>)))

{-- expression parsers --}

expr, expr', expr'' :: Parser Maybe Int
expr   = buildExpr expr   [mulop, addop]    -- normal precedence (* before +)
expr'  = buildExpr expr'  [addop, mulop]    -- inverted precedence (+ befor *)
expr'' = buildExpr expr'' [addop <|> mulop] -- no precedence

buildExpr :: Parser Maybe Int -> [Parser Maybe (Int -> Int -> Int)] -> Parser Maybe Int
buildExpr expr = foldl chainl1 $ factor expr

factor :: Parser Maybe Int -> Parser Maybe Int
factor expr = natural <|> parenthezised expr

addop, mulop :: Parser Maybe (Int -> Int -> Int)
addop = (+) <$ symbol "+" <|> (-) <$ symbol "-"
mulop = (*) <$ symbol "*" <|> div <$ symbol "/"

    -- 490511034040
    -- 21022630974613
    -- 169899524778212

main = do
    input <- readFile "day18.input"
    mapM_ (print . ($ input) . calc) [expr, expr'', expr']
      where calc parser = sum . map (maybe 0 fst . runParser parser) . lines